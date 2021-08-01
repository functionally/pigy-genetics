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
-- | Types for pig images.
--
-----------------------------------------------------------------------------


{-# LANGUAGE MultiParamTypeClasses #-}


module Pigy.Image.Types (
-- * Chromosomes
  Chromosome
-- * Phenotype
, Phenotype(..)
, Phenable(..)
-- * Upgrades
, Upgradeable(..)
) where


import Codec.Picture (PixelRGBA8(..))


-- | The chromosome.
type Chromosome = String


-- | An upgradeable object.
class Upgradeable g h where
  -- | Upgrade an object.
  upgrade :: g -- ^ The original object.
          -> h -- ^ The upgraded object.


-- | The phenotype for pig images.
data Phenotype =
  Phenotype
  {
    skinHue     :: Float          -- ^ The skin hue.
  , eyeColor    :: PixelRGBA8     -- ^ The eye color.
  , pupilColor  :: PixelRGBA8     -- ^ The pupil color.
  , noseColor   :: PixelRGBA8     -- ^ The nose color.
  , aspect      :: Float          -- ^ The overall aspect ratio.
  , headScale   :: (Float, Float) -- ^ The head scale.
  , eyeScale    :: (Float, Float) -- ^ The eye scale.
  , noseScale   :: (Float, Float) -- ^ The nose scale.
  , earScale    :: (Float, Float) -- ^ The ear scale.
  , bodyScale   :: Float          -- ^ The body scale.
  , eyeAngle    :: Float          -- ^ The radial angle of the pupil.
  , eyeFraction :: Float          -- ^ The radial position of the pupil.
  }
    deriving (Eq, Ord, Show)


-- | Conversion to phenotype.
class Phenable g where
  -- | Convert to a phenotype.
  toPhenotype :: g         -- ^ The datum.
              -> Phenotype -- ^ The phenotype.
