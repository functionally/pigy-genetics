
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}


module Pigy.Chain (
  pigy
) where


import Cardano.Api -- (AssetId(..), AsType(AsAssetName, AsPolicyId), BlockHeader(..), ConsensusModeParams(CardanoModeParams), EpochSlots(..), Network, Protocol, NetworkId(..), NetworkMagic(..), TxOut(..), TxOutValue(..), anyAddressInShelleyBasedEra, deserialiseFromRawBytes, deserialiseFromRawBytesHex, selectAsset, valueToList)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef
import Data.Maybe (mapMaybe)
import Mantis.Chain (watchTransactions)
import Mantis.Types (MantisM, foistMantisMaybe)
import Mantis.Transaction (printValueIO)
import Mantis.Wallet (readAddress, showAddressMary)

import qualified Data.ByteString.Char8 as BS (pack)
import qualified Data.Map.Strict       as M


pigy :: MonadFail m
     => MonadIO m
     => FilePath
     -> ConsensusModeParams CardanoMode
     -> NetworkId
     -> String
     -> String
     -> String
     -> MantisM m ()
pigy socket protocol network myAddress policyId assetName =
  do
    activeRef <- liftIO $ newIORef False
    sourceRef <- liftIO $ newIORef M.empty
    myAddress' <- anyAddressInShelleyBasedEra <$> readAddress myAddress
    policyId' <-
      foistMantisMaybe "Could not decode policy ID."
        . deserialiseFromRawBytesHex AsPolicyId
        $ BS.pack policyId
    assetName' <-
      foistMantisMaybe "Could not decode asset name."
        . deserialiseFromRawBytes AsAssetName
        $ BS.pack assetName
    let
      idleHandler =
        do
          writeIORef activeRef True
          readIORef sourceRef >>= print
          -- FIXME: Submit backlog of transactions.
          return False
      inHandler (BlockHeader slotNo _ _) txIn =
        do
          found <- (txIn `M.member`) <$> readIORef sourceRef
          when found
            $ do
              putStrLn ""
              putStrLn $ "Spent: " ++ show txIn ++ " during " ++ show slotNo ++ "."
              modifyIORef sourceRef
                (txIn `M.delete`)
              readIORef sourceRef >>= print . M.keys
      outHandler (BlockHeader slotNo _ _) txIns txIn (TxOut address txOutValue) =
        case txOutValue of
          TxOutValue _ value -> 
            when (selectAsset value (AssetId policyId' assetName') > 0)
              $ do
                active <- readIORef activeRef
                source <- readIORef sourceRef
                let
                  fromAddresses = mapMaybe (`M.lookup` source) txIns
                putStrLn ""
                print txIn
                putStrLn $ "  active " ++ show active
                putStrLn $ "  incoming " ++ show (showAddressMary <$> fromAddresses)
                putStrLn $ "  mine " ++ show (address == myAddress')
                putStrLn $ "  " ++ show slotNo
                putStrLn $ "  " ++ showAddressMary address
                printValueIO "  " value
                modifyIORef sourceRef
                  $ M.insert txIn address
                readIORef sourceRef >>= print . M.keys
          _ -> return ()
    watchTransactions socket protocol network idleHandler inHandler outHandler
