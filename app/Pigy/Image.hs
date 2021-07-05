module Pigy.Image (
  test
) where


import Codec.Picture(PixelRGBA8(..), writePng)
import Codec.Picture.Types (Image)
import Graphics.Rasterific (Cap(..), Drawing, Join(..), Texture, V2(..), circle, cubicBezierFromPath, fill, line, renderDrawing, roundedRectangle, stroke, withClipping, withTexture, withTransformation)
import Graphics.Rasterific.Texture (uniformTexture)
import Graphics.Rasterific.Transformations (scale, translate)
import System.Random (getStdGen)
import System.Random.Stateful (newIOGenM, randomRM)


test :: IO ()
test =
  do
    g <- newIOGenM =<< getStdGen
    aspect <- randomRM (0.75, 1.25) g
    headx  <- randomRM (0.75, 1.00) g
    heady  <- randomRM (0.75, 1.00) g
    eyex   <- randomRM (0.75, 1.00) g
    eyey   <- randomRM (0.75, 1.00) g
    nosex  <- randomRM (0.75, 1.00) g
    nosey  <- randomRM (0.75, 1.00) g
    earx   <- randomRM (0.75, 1.00) g
    eary   <- randomRM (0.75, 1.00) g
    torso  <- randomRM (0.75, 1.25) g
    writePng "yourimage.png"
      $ pigyImage
        aspect
        (headx, heady)
        (eyex , eyey )
        (nosex, nosey)
        (earx , eary )
        torso


white :: Texture PixelRGBA8
white = uniformTexture $ PixelRGBA8 0xFF 0xFF 0xFF 0xFF


black :: Texture PixelRGBA8
black = uniformTexture $ PixelRGBA8 0x00 0x00 0x00 0xFF


width :: Float
width = 245


height :: Float
height = 287


withAspect :: Float
           -> (Float, Float)
           -> Drawing px ()
           -> Drawing px ()
withAspect aspect =
  withScale
    (
      minimum [1,     aspect]
    , minimum [1, 1 / aspect]
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


pigyImage :: Float
          -> (Float, Float)
          -> (Float, Float)
          -> (Float, Float)
          -> (Float, Float)
          -> Float
          -> Image PixelRGBA8
pigyImage aspect headScale eyeScale noseScale earScale bodyScale =
  renderDrawing (round width) (round height) (PixelRGBA8 0xFF 0xFF 0xFF 0x00)
    . withAspect aspect (width / 2, height / 2)
    $ do
      let
        pink1 = uniformTexture $ PixelRGBA8 0xFF 0x57 0xA7 0xFF
        pink2 = uniformTexture $ PixelRGBA8 0xFF 0x85 0xC0 0xFF
        pink3 = uniformTexture $ PixelRGBA8 0xFF 0x70 0xB5 0xFF
        pink4 = uniformTexture $ PixelRGBA8 0xFF 0x41 0x9C 0xFF
        pink5 = uniformTexture $ PixelRGBA8 0xFF 0xAD 0xD4 0xFF
      drawBody bodyScale pink1 pink2 pink1
      withScale headScale (width / 2, 150)
        $ do
          drawHead pink3 pink4
          drawEyes eyeScale black white
          drawEars earScale pink2 pink1
          withScale noseScale (width / 2, 125)
            $ drawNose pink5 pink4 pink3 black


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
         -> Texture px
         -> Texture px
         -> Drawing px ()
drawEyes eyeScale eyeColor pupilColor =
  do
    withScale eyeScale (75, 100)
      $ do
        withTexture eyeColor
          . fill
          $ circle (V2 75.04925 101.45342) 9.72327
        withTexture pupilColor
          . fill
          $ circle (V2 70.61203 97.36941) 3.64652
    withScale eyeScale (170, 100)
      $ do
        withTexture eyeColor
          . fill
          $ circle (V2 (width - 75.04925) 101.45342) 9.72327
        withTexture pupilColor
          . fill
          $ circle (V2 (width - 70.61203) 97.36941) 3.64652


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
