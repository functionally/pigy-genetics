
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Pigy.Chain (
  pigy
) where


import Cardano.Api                                       (AddressInEra(..), AssetId(..), AssetName(..), BlockHeader(..), MaryEra, PolicyId(..), ShelleyWitnessSigningKey(..), TxIn(..), TxMetadata(..), TxMetadataValue(..), TxOut(..), TxOutValue(..), Value, filterValue, getTxId, makeScriptWitness, makeShelleyKeyWitness, makeSignedTransaction, makeTransactionBody, negateValue, selectAsset, serialiseToRawBytesHexText, valueFromList, valueToList)
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

import qualified Data.ByteString.Char8 as BS (drop, isPrefixOf, pack, unpack)
import qualified Data.Map.Strict       as M  (delete, empty, insert, lookup, member, singleton, toList)
import qualified Data.Text             as T  (pack)


pigy :: MonadFail m
     => MonadIO m
     => Context
     -> MantisM m ()
pigy context@Context{..} =
  do
    activeRef  <- liftIO $ newIORef False
    sourceRef  <- liftIO $ newIORef M.empty
    pendingRef <- liftIO $ newIORef M.empty
    let
      KeyedAddress{..} = keyedAddress
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
      inHandler (BlockHeader slotNo _ _) txIn =
        do
          found <- (txIn `M.member`) <$> readIORef sourceRef
          when found
            $ do
              putStrLn ""
              putStrLn $ show slotNo ++ ": spent " ++ show txIn
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
          putStrLn "Creating token."
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
    watchTransactions socket protocol network idleHandler inHandler outHandler


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
               printMantis $ "  Burn token: " ++ BS.unpack (head pigs)
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
               (chromosome, cid) <- liftIO $ pinImage ipfsEnv images genotype
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
                                 (TxMetaText "name"       , TxMetaText . T.pack $ "PIG " ++ chromosome)
                               , (TxMetaText "image"      , TxMetaText $ "ipfs://" <> T.pack cid      )
                               , (TxMetaText "chromosome" , TxMetaText $ T.pack chromosome            )
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
