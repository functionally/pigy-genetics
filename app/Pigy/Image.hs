
{-# LANGUAGE RecordWildCards #-}


module Pigy.Image (
  test
) where

import Codec.Picture(PixelRGBA8(..), writePng)
import Codec.Picture.Types (Image)
import Data.Binary (Binary(..), decode, encode)
import Data.Colour.RGBSpace.HSL (hsl, hslView)
import Data.Colour.SRGB (RGB(..))
import Data.Word (Word8)
import Graphics.Rasterific (Cap(..), Drawing, Join(..), Texture, V2(..), circle, cubicBezierFromPath, fill, line, renderDrawing, roundedRectangle, stroke, withClipping, withTexture, withTransformation)
import Graphics.Rasterific.Texture (uniformTexture)
import Graphics.Rasterific.Transformations (scale, translate)
import System.Random (Uniform, getStdGen)
import System.Random.Internal (uniformM, uniformRM)
import System.Random.Stateful (newIOGenM)

import qualified Data.ByteString.Lazy   as LBS
import qualified Data.ByteString.Base58 as Base58


test :: IO ()
test =
  do
    g <- newIOGenM =<< getStdGen
    genotype <- uniformM g :: IO Genotype
    let
      chromosome = encode genotype
      genotype' = decode chromosome
      phenotype = toPhenotype genotype'
      tag = init . tail . show . Base58.encodeBase58 Base58.bitcoinAlphabet $ LBS.toStrict chromosome
    writePng ("pigy-" ++ tag ++ ".png")
      $ pigyImage phenotype
    putStrLn tag


data Genotype =
  Genotype
  {
    ar     :: Float
  , headx  :: Float
  , heady  :: Float
  , eyex   :: Float
  , eyey   :: Float
  , nosex  :: Float
  , nosey  :: Float
  , earx   :: Float
  , eary   :: Float
  , torso  :: Float
  , skinh  :: Float
  , eyeh   :: Float
  , eyes   :: Float
  , eyel   :: Float
  , pupilh :: Float
  , pupils :: Float
  , pupill :: Float
  , noseh  :: Float
  , noses  :: Float
  , nosel  :: Float
  , eyea   :: Float
  , eyef   :: Float
  }
    deriving (Eq, Ord, Read, Show)

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
      put $ quantize (270 , 390 ) skinh  (0   , 360 ) eyeh   
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
      (skinh , eyeh  ) <- unquantize (270 , 390 ) (0   , 360 ) <$> get 
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
      headx  <- uniformRM (0.75, 1.00) g
      heady  <- uniformRM (0.75, 1.00) g
      eyex   <- uniformRM (0.75, 1.00) g
      eyey   <- uniformRM (0.75, 1.00) g
      nosex  <- uniformRM (0.75, 1.00) g
      nosey  <- uniformRM (0.75, 1.00) g
      earx   <- uniformRM (0.75, 1.00) g
      eary   <- uniformRM (0.75, 1.00) g
      torso  <- uniformRM (0.75, 1.25) g
      skinh  <- uniformRM (270 , 390 ) g
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


toPhenotype :: Genotype
            -> Phenotype
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


width :: Float
width = 245


height :: Float
height = 287


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


blend :: Float
      -> PixelRGBA8
      -> PixelRGBA8
blend h0 (PixelRGBA8 r1 g1 b1 _) =
  let
    (h1, s1, l1) = hslView $ RGB (fromIntegral r1 / 255) (fromIntegral g1 / 255) (fromIntegral b1 / 255)
    RGB{..} = hsl ((h0 + h1) / 2) s1 l1
    q x = round $ 255 * x
  in
    PixelRGBA8
      (q channelRed  )
      (q channelGreen)
      (q channelBlue )
      0xFF


enlarge :: Float
enlarge = 2


pigyImage :: Phenotype
          -> Image PixelRGBA8
pigyImage Phenotype{..} =
  renderDrawing (round $ enlarge * width) (round $ enlarge * height) (PixelRGBA8 0xFF 0xFF 0xFF 0x00)
    . withTransformation (scale enlarge enlarge)
    . withAspect aspect (width / 2, height / 2)
    $ do
      let
        pink1 = uniformTexture . blend skinHue $ PixelRGBA8 0xFF 0x57 0xA7 0xFF
        pink2 = uniformTexture . blend skinHue $ PixelRGBA8 0xFF 0x85 0xC0 0xFF
        pink3 = uniformTexture . blend skinHue $ PixelRGBA8 0xFF 0x70 0xB5 0xFF
        pink4 = uniformTexture . blend skinHue $ PixelRGBA8 0xFF 0x41 0x9C 0xFF
        pink5 = uniformTexture . blend skinHue $ PixelRGBA8 0xFF 0xAD 0xD4 0xFF
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
