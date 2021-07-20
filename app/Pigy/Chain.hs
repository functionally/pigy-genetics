
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Pigy.Chain (
  pigy
) where


import Cardano.Api                                       (AddressInEra(..), AssetId(..), AssetName(..), BlockHeader(..), ChainPoint, MaryEra, PolicyId(..), ShelleyWitnessSigningKey(..), SlotNo(..), TxIn(..), TxMetadata(..), TxMetadataValue(..), TxOut(..), TxOutValue(..), Value, filterValue, getTxId, makeScriptWitness, makeShelleyKeyWitness, makeSignedTransaction, makeTransactionBody, negateValue, selectAsset, serialiseToRawBytesHexText, valueFromList, valueToList)
import Control.Monad                                     (unless, when)
import Control.Monad.Error.Class                         (throwError)
import Control.Monad.IO.Class                            (MonadIO, liftIO)
import Data.IORef                                        (modifyIORef, newIORef, readIORef, writeIORef)
import Data.Maybe                                        (mapMaybe)
import Mantis.Chain                                      (watchTransactions)
import Mantis.Query                                      (submitTransaction)
import Mantis.Script                                     (mintingScript)
import Mantis.Types                                      (MantisM, foistMantisEither, printMantis, runMantisToIO)
import Mantis.Transaction                                (includeFee, makeTransaction, printValueIO, supportedMultiAsset)
import Mantis.Wallet                                     (showAddressMary)
import Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult(..))
import Pigy.Image                                        (crossover, fromChromosome, newGenotype)
import Pigy.Ipfs                                         (pinImage)
import Pigy.Types                                        (Context(..), KeyedAddress(..))

import qualified Data.ByteString.Char8 as BS  (drop, isPrefixOf, pack, unpack)
import qualified Data.Map.Strict       as M   (Map, delete, empty, insert, lookup, member, singleton, toList)
import qualified Data.Text             as T   (pack)


kSecurity :: Int
kSecurity = 2160


type History = [(SlotNo, (M.Map TxIn (AddressInEra MaryEra), M.Map TxIn (AddressInEra MaryEra, Value)))]


record :: SlotNo
       -> (M.Map TxIn (AddressInEra MaryEra), M.Map TxIn (AddressInEra MaryEra, Value))
       -> History
       -> History
record slot sourcePending history =
  take kSecurity
    $ (slot, sourcePending)
    : history


rollback :: SlotNo
         -> History
         -> History
rollback slot =
    dropWhile
      $ (/= slot)
      . fst


toSlotNo :: ChainPoint
         -> SlotNo
toSlotNo point =
  case show point of
    "ChainPointAtGenesis" -> SlotNo 0
    text                  -> SlotNo . read . takeWhile (/= ')') $ drop 19 text


pigy :: MonadFail m
     => MonadIO m
     => Context
     -> MantisM m ()
pigy context@Context{..} =
  do
    activeRef  <- liftIO $ newIORef False
    sourceRef  <- liftIO $ newIORef M.empty
    pendingRef <- liftIO $ newIORef M.empty
    historyRef <- liftIO $ newIORef [(SlotNo 0, (M.empty, M.empty))]
    slotRef    <- liftIO . newIORef $ SlotNo 0
    let
      KeyedAddress{..} = keyedAddress
      rollbackHandler point _ =
        do
          slot0 <- readIORef slotRef
          let
            slot = toSlotNo point
          putStrLn ""
          putStrLn $ "Rollback: " ++ show slot ++ " <- " ++ show slot0
          modifyIORef historyRef
            $ rollback slot
          (_, (source, pending)) <- head <$> readIORef historyRef
          writeIORef slotRef slot
          writeIORef sourceRef  source
          writeIORef pendingRef pending
      idleHandler =
        do
          active <- readIORef activeRef
          unless active
            $ do
              putStrLn ""
              putStrLn "First idling."
              writeIORef activeRef True
          pending <- readIORef pendingRef
          mapM_ (uncurry createToken)
            $ M.toList pending
          return False
      blockHandler (BlockHeader slot _ _) _ =
        do
          slot0 <- readIORef slotRef
          when verbose
            $ do
              putStrLn ""
              putStrLn $ "New block: " ++ show slot0 ++ " -> " ++ show slot
          source  <- readIORef sourceRef
          pending <- readIORef pendingRef
          modifyIORef historyRef
            $ record slot0 (source, pending)
          writeIORef slotRef slot
      inHandler (BlockHeader slot _ _) txIn =
        do
          found <- (txIn `M.member`) <$> readIORef sourceRef
          when found
            $ do
              isPending <- (txIn `M.member`) <$> readIORef pendingRef
              when (verbose || isPending)
                $ do
                  putStrLn ""
                  putStrLn $ show slot ++ ": spent " ++ show txIn
              modifyIORef sourceRef
                (txIn `M.delete`)
              modifyIORef pendingRef
                (txIn `M.delete`)
      outHandler _ inputs output (TxOut destination txOutValue) =
        case txOutValue of
          TxOutValue _ value -> 
            when (selectAsset value token > 0)
              $ do
                active <- readIORef activeRef
                source <- readIORef sourceRef
                modifyIORef sourceRef
                  $ M.insert output destination
                let
                  sources = mapMaybe (`M.lookup` source) inputs
                when (verbose || destination == keyAddress)
                  $ do
                    putStrLn ""
                    putStrLn $ "Output: " ++ show output
                    putStrLn $ "  Sources: " ++ show (showAddressMary <$> sources)
                    putStrLn $ "  Destination: " ++ showAddressMary destination
                    putStrLn $ "  To me: " ++ show (destination == keyAddress)
                    printValueIO "  " value
                when (destination == keyAddress && not (null sources))
                  $ if active
                      then createToken output (head sources, value)
                      else do
                             putStrLn "  Queued for creation."
                             modifyIORef pendingRef
                               $ M.insert output (head sources, value)
          _ -> return ()
      createToken input (destination, value) =
        do
          putStrLn ""
          putStrLn "Minting token."
          putStrLn $ "  Input: " ++ show input
          putStrLn $ "  Destination: " ++ showAddressMary destination
          putStrLn $ "  To me: "++ show (destination == keyAddress)
          putStrLn $ "  Value: " ++ show value
          modifyIORef sourceRef
            (input `M.delete`)
          modifyIORef pendingRef
            (input `M.delete`)
          unless (destination == keyAddress)
            $ do
              result <- runMantisToIO $ mint context input destination value
              case result of
                Right ()      -> return ()
                Left  message -> putStrLn $ "  " ++ message
    watchTransactions
      socket
      protocol
      network
      (Just rollbackHandler)
      idleHandler
      blockHandler
      inHandler
      outHandler


mint :: MonadFail m
     => MonadIO m
     => Context
     -> TxIn
     -> AddressInEra MaryEra
     -> Value
     -> MantisM m ()
mint Context{..} txIn destination value =
  do
    let
      KeyedAddress{..} = keyedAddress
      (script, scriptHash) = mintingScript verificationHash Nothing
      pigFilter (AssetId (PolicyId scriptHash') (AssetName name')) = scriptHash' == scriptHash && BS.isPrefixOf "PIG@" name'
      pigFilter _ = False
      pigs =
        map (\(AssetId _ (AssetName name'), _) -> BS.drop 4 name')
          . filter (pigFilter . fst)
          $ valueToList value
    (metadata, minting) <-
      if length pigs == 1
        then do
               printMantis $ "  Burnt token: " ++ BS.unpack (head pigs)
               return
                 (
                   Nothing
                 , negateValue $ filterValue pigFilter value
                 )
        else do
               genotype <-
                 liftIO
                   $ if null pigs
                       then do
                              putStrLn "  New token."
                              newGenotype gRandom
                       else do
                              putStrLn $ "  Crossover token: " ++ show (BS.unpack <$> pigs)
                              crossover gRandom $ mapMaybe (fromChromosome . BS.unpack) pigs
               (chromosome, cid) <- pinImage ipfsPin images genotype
               let
                 name = "PIG@" ++ chromosome
               return
                 (
                   Just
                     . TxMetadata
                     . M.singleton 721
                     $ TxMetaMap
                       [
                         (
                           TxMetaText . serialiseToRawBytesHexText $ scriptHash
                         , TxMetaMap
                           [
                             (
                               TxMetaText $ T.pack name
                             , TxMetaMap
                               [
                                 (TxMetaText "name"      , TxMetaText . T.pack $ "PIG " ++ chromosome                         )
                               , (TxMetaText "image"     , TxMetaText $ "ipfs://" <> T.pack cid                               )
                               , (TxMetaText "ticker"    , TxMetaText $ T.pack name                                           )
                               , (TxMetaText "parents"   , TxMetaList $ TxMetaText . T.pack . ("PIG@" ++) . BS.unpack <$> pigs)
                               , (TxMetaText "url"       , TxMetaText "https://pigy.functionally.live"                        )
                               ]
                             )
                           ]
                         )
                       ]
                 , valueFromList [(AssetId (PolicyId scriptHash) (AssetName $ BS.pack name),  1)]
                 )
    let
      value' = value <> minting
    txBody <- includeFee network pparams 1 1 1 0
      $ makeTransaction 
        [txIn]
        [TxOut destination (TxOutValue supportedMultiAsset value')]
        Nothing
        metadata
        Nothing
        (Just minting)
    txRaw <- foistMantisEither $ makeTransactionBody txBody
    let
      witness = makeShelleyKeyWitness txRaw
        $ WitnessPaymentExtendedKey signing
      witness' = makeScriptWitness script
      txSigned = makeSignedTransaction [witness, witness'] txRaw
    result <- submitTransaction socket protocol network txSigned
    case result of
      SubmitSuccess     -> printMantis $ "  Success: " ++ show (getTxId txRaw)
      SubmitFail reason -> throwError  $ "  Failure: " ++ show reason
