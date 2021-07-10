
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Pigy.Chain (
  pigy
) where


import Cardano.Api -- (BlockHeader(..), TxOut(..), TxOutValue(..), selectAsset)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (modifyIORef, newIORef, readIORef, writeIORef)
import Data.Maybe (mapMaybe)
import Mantis.Chain (watchTransactions)
import Mantis.Query (submitTransaction)
import Mantis.Script (mintingScript)
import Mantis.Types (MantisM, foistMantisEither, printMantis, runMantisToIO)
import Mantis.Transaction -- (printValueIO)
import Mantis.Wallet (showAddressMary)
import Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult(..))
import Pigy.Image (Chromosome, newChromosome)
import Pigy.Types (Context(..), KeyedAddress(..))

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict       as M
import qualified Data.Text             as T


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
          putStrLn ""
          putStrLn "Idle."
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
                -- Associate the output with the address used to fund it.
                modifyIORef sourceRef
                  $ M.insert output destination
                let
                  -- Find all of the addresses used to fund this transaction.
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
                Right chromosome -> putStrLn $ "  Created " ++ chromosome
                Left  message    -> putStrLn $ "  " ++ message
    watchTransactions socket protocol network idleHandler inHandler outHandler


mint :: MonadFail m
     => MonadIO m
     => Context
     -> TxIn
     -> AddressInEra MaryEra
     -> Value
     -> MantisM m Chromosome
mint Context{..} txIn destination value =
  do
    chromosome <- liftIO $ newChromosome gRandom
    let
      KeyedAddress{..} = keyedAddress
      (script, scriptHash) = mintingScript verificationHash Nothing
      name = "PIG@" ++ chromosome
      metadata =
        TxMetadata
          $ M.singleton 721
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
--                  , (TxMetaText "description", TxMetaText ""                             )
--                  , (TxMetaText "image"      , TxMetaText "ipfs://"                      )
                    , (TxMetaText "chromosome" , TxMetaText $ T.pack chromosome            )
                    ]
                  )
                ]
              )
            ]
      minting = valueFromList [(AssetId (PolicyId scriptHash) (AssetName $ BS.pack name), 1)]
      value' = value <> minting
    txBody <- includeFee network pparams 1 1 1 0
      $ makeTransaction 
        [txIn]
        [TxOut destination (TxOutValue supportedMultiAsset value')]
        Nothing
        (Just metadata)
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
      SubmitFail reason -> printMantis $ "  Failure: " ++ show reason
    return chromosome
