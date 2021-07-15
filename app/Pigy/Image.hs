
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}


module Pigy.Image (
  fromChromosome
, toChromosome
, newGenotype
, Genotype(..)
, toImage
, toPngBytes
, writeImage
, crossover
) where

import Codec.Picture                       (PixelRGBA8(..), encodePng, writePng)
import Codec.Picture.Types                 (Image)
import Control.Monad.IO.Class              (MonadIO, liftIO)
import Data.Binary                         (Binary(..), Get, decode, encode)
import Data.Colour.RGBSpace.HSL            (hsl)
import Data.Colour.SRGB                    (RGB(..))
import Data.Word                           (Word8)
import Graphics.Rasterific                 (Cap(..), Drawing, Join(..), Texture, V2(..), circle, cubicBezierFromPath, fill, line, renderDrawing, roundedRectangle, stroke, withClipping, withTexture, withTransformation)
import Graphics.Rasterific.Texture         (uniformTexture)
import Graphics.Rasterific.Transformations (scale, translate)
import Pigy.Image.Types                    (Chromosome, Phenable(..), Phenotype(..), Upgradeable(..))
import System.Random                       (Uniform)
import System.Random.Internal              (uniformM)
import System.Random.Stateful              (StatefulGen)

import qualified Data.ByteString        as BS     (pack, unpack)
import qualified Data.ByteString.Lazy   as LBS    (ByteString, fromStrict, toStrict)
import qualified Data.ByteString.Base58 as Base58 (bitcoinAlphabet, decodeBase58, encodeBase58)
import qualified Pigy.Image.V0          as V0     (Genotype, gid)
import qualified Pigy.Image.V1          as V1     (Genotype, crossover, gid)


data Genotype =
    GenotypeV0 V0.Genotype
  | GenotypeV1 V1.Genotype
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


newGenotype :: StatefulGen g IO
            => g
            -> IO Genotype
newGenotype = uniformM


toChromosome :: Genotype
             -> Chromosome
toChromosome =
    fmap (toEnum . fromEnum)
  . BS.unpack
  . Base58.encodeBase58 Base58.bitcoinAlphabet
  . LBS.toStrict
  . encode


fromChromosome :: Chromosome
               -> Maybe Genotype
fromChromosome text
  | length text == 15 = GenotypeV0 <$> fromChromosome' text
  | otherwise         = fromChromosome' text


fromChromosome' :: Binary a
                => Chromosome
                -> Maybe a
fromChromosome' =
    fmap (decode . LBS.fromStrict)
  . Base58.decodeBase58 Base58.bitcoinAlphabet
  . BS.pack
  . fmap (toEnum . fromEnum)


crossover :: MonadFail m
          => StatefulGen g m
          => g
          -> [Genotype]
          -> m Genotype
crossover g =
  fmap GenotypeV1
    . V1.crossover g
    . fmap upgrade


toPngBytes :: Phenotype
           -> LBS.ByteString
toPngBytes = encodePng . toImage


writeImage :: MonadIO m
           => FilePath
           -> Phenotype
           -> m ()
writeImage filename =
    liftIO
  . writePng filename
  . toImage


enlarge :: Float
enlarge = 2


width :: Float
width = 245


height :: Float
height = 287


toImage :: Phenotype
        -> Image PixelRGBA8
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


drawBody :: Float
         -> Texture px
         -> Texture px
         -> Texture px
         -> Drawing px ()
drawBody bodyScale torsoColor bellyColor bottomColor =
  do
    withTexture bottomColor
      . fill
      $ roundedRectangle (V2 16.163944 215.98409) 212.15131 99.693665 55 55
    withScale (bodyScale, 1) (width / 2, 215)
      . withTexture  torsoColor
      . fill
      $ roundedRectangle (V2 58.40379 139.59184) 126.36129 153.8943 80 80
    withTexture bellyColor
      . withClipping (fill $ roundedRectangle (V2 16.163944 215.98409) 212.15131 99.693665 55 55)
      . fill
      $ roundedRectangle (V2 58.40379 139.59184) 126.36129 153.8943 80 80


drawHead :: Texture px
         -> Texture px
         -> Drawing px ()
drawHead frontColor backColor =
  do
    withTexture backColor
      . fill
      $ roundedRectangle (V2 20.827299 50.318928) 201.86699 116.59785 60 60
    withTexture frontColor
      . fill
      $ roundedRectangle (V2 20.827299 34.955734) 201.86699 116.59785 60 60


drawEyes :: (Float, Float)
         -> (Float, Float)
         -> Texture px
         -> Texture px
         -> Drawing px ()
drawEyes (eyeFraction, eyeAngle) eyeScale eyeColor pupilColor =
  do
    withScale eyeScale (75, 100)
      $ do
        withTexture eyeColor
          . fill
          $ circle (V2 75.04925 101.45342) 9.72327
        withTexture pupilColor
          . fill
          $ circle (V2 (75.04925 + 6.03059 * eyeFraction * cos eyeAngle) (101.45342 + 6.03059 * eyeFraction * sin eyeAngle)) 3.64652
    withScale eyeScale (170, 100)
      $ do
        withTexture eyeColor
          . fill
          $ circle (V2 (width - 75.04925) 101.45342) 9.72327
        withTexture pupilColor
          . fill
          $ circle (V2 (width - 75.04925 + 6.03059 * eyeFraction * cos eyeAngle) (101.45342 + 6.03059 * eyeFraction * sin eyeAngle)) 3.64652


drawEars :: (Float, Float)
         -> Texture px
         -> Texture px
         -> Drawing px ()
drawEars earScale frontColor backColor =
  do
    withScale earScale (54, 47)
      $ do
        withTexture backColor
          . fill 
          $ cubicBezierFromPath
          [
            V2 0          0
          , V2 34.875935  0.42684743
          , V2 69.101494 15.066973
          , V2 85.434346 34.902808
          , V2 69.497156 38.440122
          , V2 51.422301 45.66022
          , V2 37.471204 58.134955
          , V2 42.774045 32.747291
          , V2 31.658189 11.934829
          , V2 0          0
          ]
        withTexture frontColor
          . fill
          $ cubicBezierFromPath
          [       
            V2 0          0
          , V2 50.861558 15.800834
          , V2 38.191333 57.31195
          , V2 37.471204 58.134955
          , V2 33.553602 63.778565
          , V2 30.631682 69.593209
          , V2 27.302137 75.122339
          , V2 14.99146  52.777337
          , V2 18.687946 21.667265
          , V2 0          0
          ]
    withScale earScale (187, 47)
      $ do
        withTexture backColor
          . fill 
          $ cubicBezierFromPath
          [
            V2  width               0
          , V2 (width - 34.875935)  0.42684743
          , V2 (width - 69.101494) 15.066973
          , V2 (width - 85.434346) 34.902808
          , V2 (width - 69.497156) 38.440122
          , V2 (width - 51.422301) 45.66022
          , V2 (width - 37.471204) 58.134955
          , V2 (width - 42.774045) 32.747291
          , V2 (width - 31.658189) 11.934829
          , V2  width               0
          ]
        withTexture frontColor
          . fill 
          $ cubicBezierFromPath
          [       
            V2  width               0
          , V2 (width - 50.861558) 15.800834
          , V2 (width - 38.191333) 57.31195
          , V2 (width - 37.471204) 58.134955
          , V2 (width - 33.553602) 63.778565
          , V2 (width - 30.631682) 69.593209
          , V2 (width - 27.302137) 75.122339
          , V2 (width - 14.99146 ) 52.777337
          , V2 (width - 18.687946) 21.667265
          , V2  width               0
          ]


drawNose :: Texture px
         -> Texture px
         -> Texture px
         -> Texture px
         -> Drawing px ()
drawNose frontColor backColor centerColor nostrilColor =
  do
    withTexture backColor
      . fill
      $ roundedRectangle (V2 86.188965 111.72396) 71.934334 39.709103 15 15
    withTexture frontColor
      . fill
      $ roundedRectangle (V2 86.188965 107.60213) 71.934334 39.709103 15 15
    withTexture centerColor
      . stroke 1 JoinRound (CapStraight 0, CapStraight 0)
      $ line (V2 122.53 107.60213) (V2 122.53 (107.60213+39.709103))
    withTexture nostrilColor
      $ do
        fill
          $ roundedRectangle (V2 101.65501 117.96757) 10.00565 16.56616 3.8053 3.8053
        fill
          $ roundedRectangle (V2 (width - 101.65501-10.00565) 117.96757) 10.00565 16.56616 3.8053 3.8053


withAspect :: Float
           -> (Float, Float)
           -> Drawing px ()
           -> Drawing px ()
withAspect ratio =
  withScale
    (
      minimum [1,     ratio]
    , minimum [1, 1 / ratio]
    )


withScale :: (Float, Float)
          -> (Float, Float)
          -> Drawing px ()
          -> Drawing px ()
withScale (sx, sy) (cx, cy) =
  withTransformation
    $  translate (V2    cx     cy )
    <> scale sx sy
    <> translate (V2 (- cx) (- cy))


skin :: Float
      -> Float
      -> PixelRGBA8
skin h l =
  let
    RGB{..} = hsl h 0.7 l
    q x = round $ 255 * x
  in
    PixelRGBA8
      (q channelRed  )
      (q channelGreen)
      (q channelBlue )
      0xFF
