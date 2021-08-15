-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- Copyright   :  (c) 2021 Brian W Bush
-- License     :  MIT
--
-- Maintainer  :  Brian W Bush <code@functionally.io>
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Minting image tokens.
--
-----------------------------------------------------------------------------


{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}


module Pigy.Chain.Mint (
-- * Minting
  mint
-- * Validation
,  checkValue
-- * Filtering
, pigFilter
, findPigs
) where


import Cardano.Api                                       (AssetId(..), AssetName(..), PolicyId(..), Quantity(..), ScriptHash, ShelleyWitnessSigningKey(..), TxIn(..), TxMetadata(..), TxMetadataValue(..), TxOut(..), TxOutValue(..), Value, filterValue, getTxId, makeScriptWitness, makeShelleyKeyWitness, makeSignedTransaction, makeTransactionBody, negateValue, quantityToLovelace, selectAsset, selectLovelace, serialiseToRawBytesHexText, valueFromList, valueToList)
import Control.Monad.Error.Class                         (throwError)
import Control.Monad.IO.Class                            (MonadIO, liftIO)
import Data.Maybe                                        (mapMaybe)
import Mantis.Query                                      (submitTransaction)
import Mantis.Script                                     (mintingScript)
import Mantis.Transaction                                (includeFee, makeTransaction, supportedMultiAsset)
import Mantis.Types                                      (MantisM, foistMantisEither, printMantis)
import Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult(..))
import Pigy.Chain.Types                                  (MaryAddress)
import Pigy.Image                                        (crossover, fromChromosome, newGenotype)
import Pigy.Ipfs                                         (pinImage)
import Pigy.Types                                        (Context(..), KeyedAddress(..))

import qualified Data.ByteString.Char8 as BS (ByteString, drop, isPrefixOf, pack, unpack)
import qualified Data.Map.Strict       as M  (fromList)
import qualified Data.Text             as T  (pack)


-- | Validate input value.
checkValue :: AssetId    -- ^ The payment token.
           -> ScriptHash -- ^ The policy for the image tokens.
           -> Value      -- ^ The value to be validated.
           -> Bool       -- ^ Whether minting can proceed.
checkValue token scriptHash value =
  let
    ada = selectLovelace value
    pigy = selectAsset value token
    pigs = maximum[1, length $ findPigs scriptHash value]
    a = 1_500_000
    b =   500_000
  in
    pigy > 0 && ada >= quantityToLovelace (Quantity $ a + b * fromIntegral pigs)


-- | Test whether an asset is a token image.
pigFilter :: ScriptHash -- ^ The policy for the image tokens.
          -> AssetId    -- ^ The asset in question.
          -> Bool       -- ^ Whether the asset is an image token.
pigFilter scriptHash (AssetId (PolicyId scriptHash') (AssetName name')) = scriptHash' == scriptHash && BS.isPrefixOf "PIG@" name'
pigFilter _ _ = False


-- | Find the tickers for the image tokens in a value.
findPigs :: ScriptHash      -- ^ The policy.
         -> Value           -- ^ The value.
         -> [BS.ByteString] -- ^ The image-token tickers.
findPigs scriptHash =
  map (\(AssetId _ (AssetName name'), _) -> BS.drop 4 name')
    . filter (pigFilter scriptHash . fst)
    . valueToList


-- | Mint or burn an image token.
mint :: MonadFail m
     => MonadIO m
     => Context      -- ^ The service context.
     -> [TxIn]       -- ^ The UTxOs to be spent.
     -> MaryAddress  -- ^ The destination.
     -> Value        -- ^ The value to be spent.
     -> [String]     -- ^ The message to be embedded in the transaction.
     -> MantisM m () -- ^ The action to mint or burn.
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
                              putStrLn $ "  Crossover token: " ++ show (("PIG@" ++) . BS.unpack <$> pigs)
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
                                 , (TxMetaText "url"       , TxMetaText "https://genetics.pigytoken.com"                        )
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
      SubmitFail reason -> do
                             printMantis $ "  Tx: " ++ show txRaw
                             throwError  $ "  Failure: " ++ show reason
