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
-- | Image blending using a genetics strategy.
--
-----------------------------------------------------------------------------


{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}


module Pigy.Image (
-- * Genotypes
  Genotype(..)
, newGenotype
, crossover
-- * Chromosomes
, fromChromosome
, toChromosome
-- * Images
, toImage
, toPngBytes
, writeImage
) where


import Codec.Picture                       (PixelRGBA8(..), encodePng, writePng)
import Codec.Picture.Types                 (Image)
import Control.Monad.IO.Class              (MonadIO, liftIO)
import Data.Binary                         (Binary(..), Get, decode, encode)
import Data.Word                           (Word8)
import Graphics.Rasterific                 (renderDrawing, withTransformation)
import Graphics.Rasterific.Texture         (uniformTexture)
import Graphics.Rasterific.Transformations (scale)
import Pigy.Image.Drawing                  (drawBody, drawEars, drawEyes, drawHead, drawNose, enlarge, height, skin, width, withAspect, withScale)
import Pigy.Image.Types                    (Chromosome, Phenable(..), Phenotype(..), Upgradeable(..))
import System.Random                       (Uniform)
import System.Random.Internal              (uniformM)
import System.Random.Stateful              (StatefulGen)

import qualified Data.ByteString        as BS     (pack, unpack)
import qualified Data.ByteString.Lazy   as LBS    (ByteString, fromStrict, toStrict)
import qualified Data.ByteString.Base58 as Base58 (bitcoinAlphabet, decodeBase58, encodeBase58)
import qualified Pigy.Image.V0          as V0     (Genotype, gid)
import qualified Pigy.Image.V1          as V1     (Genotype, crossover, gid)


-- | A versioned genotype.
data Genotype =
    GenotypeV0 V0.Genotype -- ^ Version 0.
  | GenotypeV1 V1.Genotype -- ^ Version 1.
    deriving (Eq, Ord, Show)

instance Upgradeable Genotype Genotype where
  upgrade (GenotypeV0 g) = GenotypeV1 $ upgrade g
  upgrade (GenotypeV1 g) = GenotypeV1 g

instance Upgradeable Genotype V1.Genotype where
  upgrade (GenotypeV0 g) = upgrade g
  upgrade (GenotypeV1 g) = g

instance Phenable Genotype where
  toPhenotype (GenotypeV0 g) = toPhenotype g
  toPhenotype (GenotypeV1 g) = toPhenotype g

instance Binary Genotype where
  put (GenotypeV0 g) = put V0.gid >> put g
  put (GenotypeV1 g) = put V1.gid >> put g
  get = do
          gid <- get :: Get Word8
          if gid == V1.gid
            then GenotypeV1 <$> get
            else if gid == V0.gid
                    then GenotypeV0 <$> get
                    else fail "Invalid genome."

instance Uniform Genotype where
  uniformM = fmap GenotypeV1 . uniformM


-- | Create a new genotype at random.
newGenotype :: StatefulGen g IO
            => g           -- ^ The random-number generator.
            -> IO Genotype -- ^ The action to create the genotype.
newGenotype = uniformM


-- | Convert a genotype to a chromosome.
toChromosome :: Genotype   -- ^ The genotype.
             -> Chromosome -- ^ The chromosome.
toChromosome =
    fmap (toEnum . fromEnum)
  . BS.unpack
  . Base58.encodeBase58 Base58.bitcoinAlphabet
  . LBS.toStrict
  . encode


-- | Convert a chromosome to a genotype.
fromChromosome :: Chromosome     -- ^ The chromosome.
               -> Maybe Genotype -- ^ The genotype, if the chromosome was valid.
fromChromosome text
  | length text == 15 = GenotypeV0 <$> fromChromosome' text
  | otherwise         = fromChromosome' text


-- | Convert a chromosome to something serializable.
fromChromosome' :: Binary a
                => Chromosome -- ^ The chromosome.
                -> Maybe a    -- ^ The serializable, if the chromosome was valid.
fromChromosome' =
    fmap (decode . LBS.fromStrict)
  . Base58.decodeBase58 Base58.bitcoinAlphabet
  . BS.pack
  . fmap (toEnum . fromEnum)


-- | Perform crossover between genotypes.
crossover :: MonadFail m
          => StatefulGen g m
          => g          -- ^ The random-number generator.
          -> [Genotype] -- ^ The genotypes to be crossed.
          -> m Genotype -- ^ The action to cross the genotypes.
crossover g =
  fmap GenotypeV1
    . V1.crossover g
    . fmap upgrade


-- | Convert a phenotype to a PNG image.
toPngBytes :: Phenotype      -- ^ The phenotype.
           -> LBS.ByteString -- ^ The PNG image bytes.
toPngBytes = encodePng . toImage


-- | Write a phenotype to a PNG file.
writeImage :: MonadIO m
           => FilePath  -- ^ The path to the PNG file.
           -> Phenotype -- ^ The phenotype.
           -> m ()      -- ^ The action to write the PNG file.
writeImage filename =
    liftIO
  . writePng filename
  . toImage


-- | Convert a phenotype to an image.
toImage :: Phenotype        -- ^ The phenotype.
        -> Image PixelRGBA8 -- ^ The image.
toImage Phenotype{..} =
  renderDrawing (round $ enlarge * width) (round $ enlarge * height) (PixelRGBA8 0xFF 0xFF 0xFF 0x00)
    . withTransformation (scale enlarge enlarge)
    . withAspect aspect (width / 2, height / 2)
    $ do
      let
        pink1 = uniformTexture $ skin skinHue 0.67
        pink2 = uniformTexture $ skin skinHue 0.76
        pink3 = uniformTexture $ skin skinHue 0.72
        pink4 = uniformTexture $ skin skinHue 0.63
        pink5 = uniformTexture $ skin skinHue 0.84
      drawBody bodyScale pink1 pink2 pink1
      withScale headScale (width / 2, 150)
        $ do
          drawHead pink3 pink4
          drawEyes (eyeFraction, eyeAngle) eyeScale (uniformTexture eyeColor) (uniformTexture pupilColor)
          drawEars earScale pink2 pink1
          withScale noseScale (width / 2, 125)
            $ drawNose pink5 pink4 pink3 (uniformTexture noseColor)
