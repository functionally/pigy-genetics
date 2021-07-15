
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}


module Pigy.Image.Types (
  Chromosome
, Upgradeable(..)
, Phenotype(..)
, Phenable(..)
) where


import Codec.Picture (PixelRGBA8(..))


type Chromosome = String


class Upgradeable g h where
  upgrade :: g -> h


data Phenotype =
  Phenotype
  {
    skinHue     :: Float
  , eyeColor    :: PixelRGBA8
  , pupilColor  :: PixelRGBA8
  , noseColor   :: PixelRGBA8
  , aspect      :: Float
  , headScale   :: (Float, Float)
  , eyeScale    :: (Float, Float)
  , noseScale   :: (Float, Float)
  , earScale    :: (Float, Float)
  , bodyScale   :: Float
  , eyeAngle    :: Float
  , eyeFraction :: Float
  }
    deriving (Eq, Ord, Show)


class Phenable g where
  toPhenotype :: g -> Phenotype
