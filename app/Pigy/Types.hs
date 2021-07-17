
{-# LANGUAGE RecordWildCards #-}


module Pigy.Types (
  Configuration(..)
, KeyInfo(..)
, readConfiguration
, Context(..)
, makeContext
, KeyedAddress(..)
, readKeyedAddress
) where


import Cardano.Api            (AddressInEra(..), AssetId(..), AsType (AsAssetName, AsPolicyId), CardanoMode, ConsensusModeParams(CardanoModeParams), EpochSlots(..), Hash, MaryEra, NetworkId(..), NetworkMagic(..), PaymentKey, PaymentExtendedKey, SigningKey, anyAddressInShelleyBasedEra, deserialiseFromRawBytes, deserialiseFromRawBytesHex)
import Cardano.Api.Shelley    (ProtocolParameters)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Word              (Word32, Word64)
import Mantis.Query           (queryProtocol)
import Mantis.Types           (MantisM, foistMantisMaybe)
import Mantis.Wallet          (SomePaymentVerificationKey, makeVerificationKeyHash, readAddress, readSigningKey, readVerificationKey)
import System.Random          (StdGen, getStdGen)
import System.Random.Stateful (IOGenM, newIOGenM)

import qualified Data.ByteString.Char8 as BS (pack)


data Configuration =
  Configuration
  {
    socketPath  :: FilePath
  , magic       :: Maybe Word32
  , epochSlots  :: Word64
  , policyId    :: String
  , assetName   :: String
  , keyInfo     :: KeyInfo
  , ipfsScript  :: FilePath
  , imageFolder :: FilePath
  , quiet       :: Bool
  }
    deriving (Read, Show)


data KeyInfo =
  KeyInfo
  {
    addressString       :: String
  , verificationKeyFile :: FilePath
  , signingKeyFile      :: FilePath
  }
    deriving (Read, Show)


data Context =
  Context
  {
    socket       :: FilePath
  , protocol     :: ConsensusModeParams CardanoMode
  , network      :: NetworkId
  , pparams      :: ProtocolParameters
  , token        :: AssetId
  , keyedAddress :: KeyedAddress
  , gRandom      :: IOGenM StdGen
  , ipfsPin      :: FilePath
  , images       :: FilePath
  , verbose      :: Bool
  }


data KeyedAddress =
  KeyedAddress
  {
    keyAddress       :: AddressInEra MaryEra
  , verificationHash :: Hash PaymentKey
  , verification     :: SomePaymentVerificationKey
  , signing          :: SigningKey PaymentExtendedKey
  }
    deriving (Show)


readConfiguration :: MonadIO m
                  => FilePath
                  -> MantisM m Configuration
readConfiguration = liftIO . fmap read . readFile


makeContext :: MonadFail m
            => MonadIO m
            => Configuration
            -> MantisM m Context
makeContext Configuration{..} =
  do
    policyId' <-
      foistMantisMaybe "Could not decode policy ID."
        . deserialiseFromRawBytesHex AsPolicyId
        $ BS.pack policyId
    assetName' <-
      foistMantisMaybe "Could not decode asset name."
        . deserialiseFromRawBytes AsAssetName
        $ BS.pack assetName
    keyedAddress <- readKeyedAddress keyInfo
    gRandom <- newIOGenM =<< getStdGen
    let
      socket = socketPath
      protocol = CardanoModeParams $ EpochSlots epochSlots
      network = maybe Mainnet (Testnet . NetworkMagic) magic
      token = AssetId policyId' assetName'
      ipfsPin = ipfsScript
      images = imageFolder
      verbose = not quiet
    pparams <- queryProtocol socketPath protocol network
    return Context{..}


readKeyedAddress :: MonadIO m
                 => KeyInfo
                 -> MantisM m KeyedAddress
readKeyedAddress KeyInfo{..} =
  do
    keyAddress <- anyAddressInShelleyBasedEra <$> readAddress addressString
    verification <- readVerificationKey verificationKeyFile
    signing <- readSigningKey signingKeyFile
    let
      verificationHash = makeVerificationKeyHash verification
    return KeyedAddress{..}
