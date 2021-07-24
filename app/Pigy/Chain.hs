
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}


module Pigy.Chain (
  runChain
) where


import Cardano.Api                                       (AddressInEra(..), AssetId(..), AssetName(..), BlockHeader(..), ChainPoint, MaryEra, PolicyId(..), Quantity(..), ScriptHash, ShelleyWitnessSigningKey(..), SlotNo(..), StakeAddressReference(NoStakeAddress), TxIn(..), TxMetadata(..), TxMetadataValue(..), TxOut(..), TxOutValue(..), Value, filterValue, getTxId, makeScriptWitness, makeShelleyKeyWitness, makeSignedTransaction, makeTransactionBody, negateValue, quantityToLovelace, selectAsset, selectLovelace, serialiseToRawBytesHexText, valueFromList, valueToList)
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
import Mantis.Wallet                                     (showAddressMary, stakeReferenceMary)
import Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult(..))
import Pigy.Image                                        (crossover, fromChromosome, newGenotype)
import Pigy.Ipfs                                         (pinImage)
import Pigy.Types                                        (Context(..), KeyedAddress(..))

import qualified Data.ByteString.Char8 as BS  (ByteString, drop, isPrefixOf, pack, unpack)
import qualified Data.Map.Strict       as M   (Map, delete, empty, fromList, fromListWith, insert, lookup, member, toList)
import qualified Data.Text             as T   (pack)


kSecurity :: Int
kSecurity = 2160


type History = [(SlotNo, (M.Map TxIn (AddressInEra MaryEra), M.Map TxIn ([AddressInEra MaryEra], Value)))]


record :: SlotNo
       -> (M.Map TxIn (AddressInEra MaryEra), M.Map TxIn ([AddressInEra MaryEra], Value))
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


runChain :: MonadFail m
         => MonadIO m
         => Context
         -> MantisM m ()
runChain context@Context{..} =
  do
    activeRef  <- liftIO $ newIORef False
    sourceRef  <- liftIO $ newIORef M.empty
    pendingRef <- liftIO $ newIORef M.empty
    historyRef <- liftIO $ newIORef [(SlotNo 0, (M.empty, M.empty))]
    slotRef    <- liftIO . newIORef $ SlotNo 0
    let
      KeyedAddress{..} = keyedAddress
      (_, scriptHash) = mintingScript verificationHash Nothing
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
          do
            pending <- readIORef pendingRef
            sequence_
              [
                createToken [output] (head sources, value)
              |
                (output, (sources, value)) <- M.toList pending
              , checkValue token scriptHash value
              ]
          do
            pending <- readIORef pendingRef
            sequence_
              [
                do
                  when (verbose || valid)
                    $ do
                      putStrLn ""
                      putStrLn "Multiple input transactions:"
                      putStrLn $ "  Stake: " ++ stake
                      sequence_
                        [
                          putStrLn $ "  Source: " ++ show (showAddressMary source')
                        |
                          source' <- sources
                        ]
                      putStrLn $ "  Valid: " ++ show valid
                      printValueIO "  " value
                  when valid
                    $ createToken outputs (head sources, value)
              |
                let pending' = M.fromListWith
                                 (
                                   \(outputs, sources, value) (outputs', sources', value') ->
                                     (
                                       outputs <> outputs'
                                     , sources <> sources'
                                     , value   <> value'
                                     )
                                 )
                                 . map
                                 (
                                   \(output, (sources, value)) ->
                                     (
                                       show . stakeReferenceMary $ head sources
                                     , (
                                         [output]
                                       , sources
                                       , value
                                       )
                                     )
                                 )
                                 $ M.toList pending
              , (stake, (outputs, sources, value)) <- M.toList pending'
              , let valid = checkValue token scriptHash value
              , stake /= show NoStakeAddress
              ]
          return False
      blockHandler (BlockHeader slot _ _) _ =
        do
          active <- readIORef activeRef
          slot0 <- readIORef slotRef
          when (verbose && active)
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
            when (selectAsset value token > 0 || destination == keyAddress)
              $ do
                active <- readIORef activeRef
                source <- readIORef sourceRef
                modifyIORef sourceRef
                  $ M.insert output destination
                let
                  sources = mapMaybe (`M.lookup` source) inputs
                  valid = checkValue token scriptHash value
                when (verbose || destination == keyAddress)
                  $ do
                    putStrLn ""
                    putStrLn $ "Output: " ++ show output
                    sequence_
                      [
                        putStrLn $ "  Source: " ++ show (showAddressMary source')
                      |
                        source' <- sources
                      ]
                    putStrLn $ "  Destination: " ++ showAddressMary destination
                    putStrLn $ "  Stake: " ++ show (stakeReferenceMary destination)
                    putStrLn $ "  To me: " ++ show (destination == keyAddress)
                    putStrLn $ "  Valid: " ++ show valid
                    printValueIO "  " value
                when (destination == keyAddress && not (null sources))
                  $ if active && valid
                      then createToken [output] (head sources, value)
                      else do
                             putStrLn "  Queued for creation."
                             modifyIORef pendingRef
                               $ M.insert output (sources, value)
          _ -> return ()
      createToken inputs (destination, value) =
        do
          putStrLn ""
          putStrLn "Minting token."
          sequence_
            [
              putStrLn $ "  Input: " ++ show input
            |
              input <- inputs
            ]
          putStrLn $ "  Destination: " ++ showAddressMary destination
          putStrLn $ "  To me: "++ show (destination == keyAddress)
          printValueIO "  " value
          sequence_
            [
              -- FIXME: Consider deleting the transactions only if the minting succeeeds.
              do
                modifyIORef sourceRef
                  (input `M.delete`)
                modifyIORef pendingRef
                  (input `M.delete`)
            |
              input <-inputs
            ]
          unless (destination == keyAddress)
            $ do
              let
                message =
                  case (selectAsset value token, length inputs) of
                    (1, 1) -> [
                                "Thank you!"
                              ]
                    (1, _) -> [
                                "Please send the tokens and ADA in a single transaction."
                              ]
                    (_, 1) -> [
                                "There is a limit of one minting per transaction."
                              , "Sending more that one PIGY does not mint more pig images."
                              ]
                    (_, _) -> [
                                "There is a limit of one minting per transaction."
                              , "Sending more that one PIGY does not mint more pig images."
                              , "Also, please send the tokens and ADA in a single transaction."
                              ]
              result <- runMantisToIO $ mint context inputs destination value message
              case result of
                Right () -> return ()
                Left  e  -> putStrLn $ "  " ++ e
    watchTransactions
      socket
      protocol
      network
      (Just rollbackHandler)
      idleHandler
      blockHandler
      inHandler
      outHandler


checkValue :: AssetId
           -> ScriptHash
           -> Value
           -> Bool
checkValue token scriptHash value =
  let
    ada = selectLovelace value
    pigy = selectAsset value token
    pigs = maximum[1, length $ findPigs scriptHash value]
    a = 1_500_000
    b =   500_000
  in
    pigy > 0 && ada >= quantityToLovelace (Quantity $ a + b * fromIntegral pigs)


pigFilter :: ScriptHash
          -> AssetId
          -> Bool
pigFilter scriptHash (AssetId (PolicyId scriptHash') (AssetName name')) = scriptHash' == scriptHash && BS.isPrefixOf "PIG@" name'
pigFilter _ _ = False


findPigs :: ScriptHash
         -> Value
         -> [BS.ByteString]
findPigs scriptHash =
  map (\(AssetId _ (AssetName name'), _) -> BS.drop 4 name')
    . filter (pigFilter scriptHash . fst)
    . valueToList


mint :: MonadFail m
     => MonadIO m
     => Context
     -> [TxIn]
     -> AddressInEra MaryEra
     -> Value
     -> [String]
     -> MantisM m ()
mint Context{..} txIns destination value message =
  do
    let
      KeyedAddress{..} = keyedAddress
      (script, scriptHash) = mintingScript verificationHash Nothing
      pigs = findPigs scriptHash value
    (metadata, minting) <-
      if length pigs == 1
        then do
               printMantis $ "  Burnt token: " ++ BS.unpack (head pigs)
               return
                 (
                   Nothing
                 , negateValue $ filterValue (pigFilter scriptHash) value
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
                     $ M.fromList
                     [
                       (
                         721
                       , TxMetaMap
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
                       )
                     , (
                         674
                       , TxMetaMap
                         [
                           (
                             TxMetaText "msg"
                           , TxMetaList $ TxMetaText . T.pack <$> message
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
        txIns
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
