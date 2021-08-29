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
-- | Types for the pig-image service.
--
-----------------------------------------------------------------------------


{-# LANGUAGE RecordWildCards #-}


module Pigy.Types (
-- * Configuration
  Configuration(..)
, readConfiguration
-- * Context
, Context(..)
, makeContext
-- * Operations
, Mode(..)
-- * Keys
, KeyInfo(..)
, KeyedAddress(..)
, readKeyedAddress
) where


import Cardano.Api            (AddressAny, AssetId(..), AsType (AsAssetName, AsPolicyId), CardanoMode, ConsensusModeParams(CardanoModeParams), EpochSlots(..), Hash, NetworkId(..), NetworkMagic(..), PaymentKey, ShelleyBasedEra(..), deserialiseFromRawBytes, deserialiseFromRawBytesHex)
import Cardano.Api.Shelley    (ProtocolParameters)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Word              (Word32, Word64)
import Mantis.Query           (queryProtocol)
import Mantis.Types           (MantisM, foistMantisMaybe)
import Mantis.Wallet          (SomePaymentSigningKey, SomePaymentVerificationKey, makeVerificationKeyHash, readAddress, readSigningKey, readVerificationKey)
import System.Random          (StdGen, getStdGen)
import System.Random.Stateful (IOGenM, newIOGenM)

import qualified Data.ByteString.Char8 as BS (pack)


-- | The service configuration.
data Configuration =
  Configuration
  {
    socketPath  :: FilePath     -- ^ The path for the Cardano node's socket.
  , magic       :: Maybe Word32 -- ^ The magic number for the Cardano network, unless using mainnet.
  , epochSlots  :: Word64       -- ^ The number of slots per epoch.
  , rollbacks   :: Int          -- ^ The number of rollbacks to allow.
  , policyId    :: String       -- ^ The policy ID of the payment token.
  , assetName   :: String       -- ^ The asset name of the payment token.
  , keyInfo     :: KeyInfo      -- ^ The service's key.
  , ipfsScript  :: FilePath     -- ^ The path to the IPFS pinning script.
  , imageFolder :: FilePath     -- ^ The path to the folder of images.
  , mode        :: Mode         -- ^ The operational mode.
  , quiet       :: Bool         -- ^ The verbosity.
  }
    deriving (Read, Show)


-- | The operational mode.
data Mode =
    Strict     -- ^ Only accept requests as single transactions.
  | Lenient    -- ^ Accept split transactions, processing when idle.
  | Aggressive -- ^ Accept split transactions, processing as soon as possible.
    deriving (Eq, Ord, Read, Show)


-- | Key information.
data KeyInfo =
  KeyInfo
  {
    addressString       :: String   -- ^ The address.
  , verificationKeyFile :: FilePath -- ^ The path to the verification key file.
  , signingKeyFile      :: FilePath -- ^ The path to the signing key file.
  }
    deriving (Read, Show)


-- | The contetual parameters for the service.
data Context =
  Context
  {
    socket       :: FilePath                        -- ^ The path for the Cardano node's socket.
  , protocol     :: ConsensusModeParams CardanoMode -- ^ The Cardano consensus mode.
  , network      :: NetworkId                       -- ^ The Cardano network.
  , pparams      :: ProtocolParameters              -- ^ The Cardano protocol.
  , kSecurity    :: Int                             -- ^ The number of rollbacks to allow.
  , token        :: AssetId                         -- ^ The asset ID for the payment token.
  , keyedAddress :: KeyedAddress                    -- ^ The service address.
  , gRandom      :: IOGenM StdGen                   -- ^ The random-number generator.
  , ipfsPin      :: FilePath                        -- ^ The path to the IPFS script for pinning images.
  , images       :: FilePath                        -- ^ The path to the folder for images.
  , operation    :: Mode                            -- ^ The operational mode.
  , verbose      :: Bool                            -- ^ The verbosity.
  }


-- | A key and it hashes.
data KeyedAddress =
  KeyedAddress
  {
    keyAddress       :: AddressAny                 -- ^ The address.
  , verificationHash :: Hash PaymentKey            -- ^ The hash of the verification key.
  , verification     :: SomePaymentVerificationKey -- ^ The verification key.
  , signing          :: SomePaymentSigningKey      -- ^ The signing key.
  }
    deriving (Show)


-- | Read a configuration file.
readConfiguration :: MonadIO m
                  => FilePath                -- ^ The path to the configuration file.
                  -> MantisM m Configuration -- ^ The action returning the configuration.
readConfiguration = liftIO . fmap read . readFile


-- | Convert a configuration into a service context.
makeContext :: MonadFail m
            => MonadIO m
            => Configuration     -- ^ The configuration.
            -> MantisM m Context -- ^ The action returning the context.
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
      kSecurity = rollbacks
      token = AssetId policyId' assetName'
      ipfsPin = ipfsScript
      images = imageFolder
      operation = mode
      verbose = not quiet
    pparams <- queryProtocol ShelleyBasedEraAlonzo socketPath protocol network
    return Context{..}


-- | Read a key.
readKeyedAddress :: MonadIO m
                 => KeyInfo                -- ^ The key information.
                 -> MantisM m KeyedAddress -- ^ The key and its hashes.
readKeyedAddress KeyInfo{..} =
  do
    keyAddress <- readAddress addressString
    verification <- readVerificationKey verificationKeyFile
    signing <- readSigningKey signingKeyFile
    let
      verificationHash = makeVerificationKeyHash verification
    return KeyedAddress{..}
