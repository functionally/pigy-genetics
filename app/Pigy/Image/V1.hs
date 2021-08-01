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
-- | Version 1 of the pig-image genotype and phenotype.
--
-----------------------------------------------------------------------------


{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}


module Pigy.Image.V1 (
-- * Versioning
  gid
-- * Genetics
, Genotype(..)
, Phenotype(..)
, crossover
) where


import Codec.Picture            (PixelRGBA8(..))
import Control.Monad            (replicateM)
import Data.Binary              (Binary(..))
import Data.Colour.RGBSpace.HSL (hsl)
import Data.Colour.SRGB         (RGB(..))
import Data.Fixed               (mod')
import Data.Word                (Word8)
import Pigy.Image.Types         (Phenable(..), Phenotype(..), Upgradeable(..))
import System.Random            (Uniform)
import System.Random.Internal   (uniformM, uniformRM)
import System.Random.Stateful   (StatefulGen)


-- | The version of the genotype.
gid :: Word8
gid = 1


data Genotype =
  Genotype
  {
    ar     :: Float -- ^ Aspect ratio.
  , headx  :: Float -- ^ Head width.
  , heady  :: Float -- ^ Head height.
  , eyex   :: Float -- ^ Eye width.
  , eyey   :: Float -- ^ Eye height.
  , nosex  :: Float -- ^ Nose width.
  , nosey  :: Float -- ^ Nose height.
  , earx   :: Float -- ^ Ear width.
  , eary   :: Float -- ^ Ear height.
  , torso  :: Float -- ^ Torso size.
  , skinh  :: Float -- ^ Skin hue.
  , eyeh   :: Float -- ^ Eye hue.
  , eyes   :: Float -- ^ Eye saturation.
  , eyel   :: Float -- ^ Eye luminosity.
  , pupilh :: Float -- ^ Pupil hue.
  , pupils :: Float -- ^ Pupil saturation.
  , pupill :: Float -- ^ Pupil luminosity.
  , noseh  :: Float -- ^ Nose hue.
  , noses  :: Float -- ^ Nose saturation.
  , nosel  :: Float -- ^ Nose luminosity.
  , eyea   :: Float -- ^ Pupil radial angle.
  , eyef   :: Float -- ^ Pupil radial position.
  }
    deriving (Eq, Ord, Read, Show)

instance Upgradeable Genotype Genotype where
  upgrade = id

instance Phenable Genotype where
  toPhenotype Genotype{..} =
    let
      hsl2rgb h s l =
        let
          RGB{..} = hsl h s l
          q x = round $ 255 * x
        in
          PixelRGBA8 (q channelRed) (q channelGreen) (q channelBlue) 0xFF
      skinHue     = skinh
      eyeColor    = hsl2rgb eyeh  eyes  eyel
      pupilColor  = hsl2rgb pupilh pupils pupill
      noseColor   = hsl2rgb noseh  noses  nosel
      aspect      = ar
      headScale   = (headx, heady)
      eyeScale    = (eyex , eyey )
      noseScale   = (nosex, nosey)
      earScale    = (earx , eary )
      bodyScale   = torso
      eyeAngle    = eyea * pi / 180
      eyeFraction = eyef
    in
      Phenotype{..}

instance Binary Genotype where
  put Genotype{..} =
    do
      let
        quantize :: (Float, Float) -> Float -> (Float, Float) -> Float -> Word8
        quantize (x0, x1) x (y0, y1) y =
          let
            hi = round $ 15 * (x - x0) / (x1 - x0)
            lo = round $ 15 * (y - y0) / (y1 - y0)
          in
            16 * hi + lo
      put $ quantize (0.75, 1.25) ar     (0.75, 1.25) torso
      put $ quantize (0.75, 1.00) headx  (0.75, 1.00) heady
      put $ quantize (0.75, 1.00) eyex   (0.75, 1.00) eyey
      put $ quantize (0.75, 1.00) nosex  (0.75, 1.00) nosey
      put $ quantize (0.75, 1.00) earx   (0.75, 1.00) eary
      put $ quantize (0   , 360 ) skinh  (0   , 360 ) eyeh   
      put $ quantize (0.80, 1.00) eyes   (0.65, 1.00) eyel   
      put $ quantize (0   , 360 ) pupilh (0.80, 1.00) pupils
      put $ quantize (0.00, 0.35) pupill (0   , 360 ) noseh  
      put $ quantize (0.80, 1.00) noses  (0.00, 0.40) nosel  
      put $ quantize (0   , 360 ) eyea   (0.2 , 1   ) eyef
  get =
    do
      let
        unquantize :: (Float, Float) -> (Float, Float) -> Word8 -> (Float, Float)
        unquantize (x0, x1) (y0, y1) w =
          let
            (hi, lo) = w `divMod` 16
          in
            (
              x0 + (x1 - x0) * fromIntegral hi / 15
            , y0 + (y1 - y0) * fromIntegral lo / 15
            )
      (ar    , torso ) <- unquantize (0.75, 1.25) (0.75, 1.25) <$> get
      (headx , heady ) <- unquantize (0.75, 1.00) (0.75, 1.00) <$> get
      (eyex  , eyey  ) <- unquantize (0.75, 1.00) (0.75, 1.00) <$> get
      (nosex , nosey ) <- unquantize (0.75, 1.00) (0.75, 1.00) <$> get
      (earx  , eary  ) <- unquantize (0.75, 1.00) (0.75, 1.00) <$> get
      (skinh , eyeh  ) <- unquantize (0   , 360 ) (0   , 360 ) <$> get 
      (eyes  , eyel  ) <- unquantize (0.80, 1.00) (0.65, 1.00) <$> get 
      (pupilh, pupils) <- unquantize (0   , 360 ) (0.80, 1.00) <$> get 
      (pupill, noseh ) <- unquantize (0.00, 0.35) (0   , 360 ) <$> get 
      (noses , nosel ) <- unquantize (0.80, 1.00) (0.00, 0.40) <$> get 
      (eyea  , eyef  ) <- unquantize (0   , 360 ) (0.2 , 1   ) <$> get
      return Genotype{..}

instance Uniform Genotype where
  uniformM g =
    do
      ar     <- uniformRM (0.75, 1.25) g
      torso  <- uniformRM (0.75, 1.25) g
      headx  <- uniformRM (0.75, 1.00) g
      heady  <- uniformRM (0.75, 1.00) g
      eyex   <- uniformRM (0.75, 1.00) g
      eyey   <- uniformRM (0.75, 1.00) g
      nosex  <- uniformRM (0.75, 1.00) g
      nosey  <- uniformRM (0.75, 1.00) g
      earx   <- uniformRM (0.75, 1.00) g
      eary   <- uniformRM (0.75, 1.00) g
      skinh  <- uniformRM (0   , 360 ) g
      eyeh   <- uniformRM (0   , 360 ) g
      eyes   <- uniformRM (0.80, 1.00) g
      eyel   <- uniformRM (0.65, 1.00) g
      pupilh <- uniformRM (0   , 360 ) g
      pupils <- uniformRM (0.80, 1.00) g
      pupill <- uniformRM (0.00, 0.35) g
      noseh  <- uniformRM (0   , 360 ) g
      noses  <- uniformRM (0.80, 1.00) g
      nosel  <- uniformRM (0.00, 0.40) g
      eyea   <- uniformRM (0   , 360 ) g
      eyef   <- uniformRM (0.2 , 1   ) g
      return Genotype{..}


-- | Perform crossover between genotypes.
crossover :: MonadFail m
          => StatefulGen g m
          => g          -- ^ The random-number generator.
          -> [Genotype] -- ^ The genotypes to be crossed.
          -> m Genotype -- ^ The action to cross the genotypes.
crossover g genotypes =
  do
    let
      blend f h0 h1 =
        let
          delta = h1 - h0
          delta' =
            if abs delta < 180
              then delta
              else delta - 360
        in
          (h0 + f * delta') `mod'` 360
      n = length genotypes
    [ar', headx', heady', eyex', eyey', nosex', nosey', earx', eary', torso', eyeh', eyes', eyel', pupilh', pupils', pupill', noseh', noses', nosel', eyea', eyef'] <- replicateM 21 $ uniformRM (0, n-1) g
    [skinh'] <- replicateM 1 $ uniformRM (0, 1) g
    return
      $ Genotype
      {
        ar      = ar     $ genotypes !! ar'    
      , headx   = headx  $ genotypes !! headx' 
      , heady   = heady  $ genotypes !! heady' 
      , eyex    = eyex   $ genotypes !! eyex'  
      , eyey    = eyey   $ genotypes !! eyey'  
      , nosex   = nosex  $ genotypes !! nosex' 
      , nosey   = nosey  $ genotypes !! nosey' 
      , earx    = earx   $ genotypes !! earx'  
      , eary    = eary   $ genotypes !! eary'  
      , torso   = torso  $ genotypes !! torso' 
      , skinh   = foldl1 (blend skinh') $ skinh <$> genotypes
      , eyeh    = eyeh   $ genotypes !! eyeh'  
      , eyes    = eyes   $ genotypes !! eyes'  
      , eyel    = eyel   $ genotypes !! eyel'  
      , pupilh  = pupilh $ genotypes !! pupilh'
      , pupils  = pupils $ genotypes !! pupils'
      , pupill  = pupill $ genotypes !! pupill'
      , noseh   = noseh  $ genotypes !! noseh' 
      , noses   = noses  $ genotypes !! noses' 
      , nosel   = nosel  $ genotypes !! nosel' 
      , eyea    = eyea   $ genotypes !! eyea'  
      , eyef    = eyef   $ genotypes !! eyef'  
      }
