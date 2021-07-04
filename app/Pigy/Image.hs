module Pigy.Image (
  test
) where


import Codec.Picture(PixelRGBA8(..), writePng)
import Codec.Picture.Types (Image)
import Graphics.Rasterific (Cap(..), Drawing, Join(..), V2(..), circle, cubicBezierFromPath, fill, line, renderDrawing, roundedRectangle, stroke, withClipping, withTexture)
import Graphics.Rasterific.Texture (uniformTexture)


test :: IO ()
test = writePng "yourimage.png" pigyImage


white :: PixelRGBA8
white = PixelRGBA8 0xFF 0xFF 0xFF 0xFF


black :: PixelRGBA8
black = PixelRGBA8 0x00 0x00 0x00 0xFF


pigyImage :: Image PixelRGBA8
pigyImage =
  renderDrawing 245 287 white
    $ do
      drawBody
        (PixelRGBA8 0xff 0x57 0xa7 0xff)
        (PixelRGBA8 0xff 0x85 0xc0 0xff)
        (PixelRGBA8 0xff 0x57 0xa7 0xff)
      drawHead
        (PixelRGBA8 0xff 0x70 0xb5 0xff)
        (PixelRGBA8 0xff 0x41 0x9c 0xff)
      drawEyes       black white
      drawEars
        (PixelRGBA8 0xff 0x8a 0xc2 0xff)
        (PixelRGBA8 0xff 0x57 0xa7 0xff)
      drawNose
        (PixelRGBA8 0xff 0xad 0xd4 0xff)
        (PixelRGBA8 0xff 0x31 0x94 0xff)
        (PixelRGBA8 0xff 0x70 0xb5 0xff)
        black


drawBody :: px -> px -> px -> Drawing px ()
drawBody torsoColor bellyColor bottomColor =
  do
    withTexture (uniformTexture bottomColor)
      . fill
      $ roundedRectangle (V2 16.163944 215.98409) 212.15131 99.693665 55 55
    withTexture (uniformTexture torsoColor)
      . fill
      $ roundedRectangle (V2 58.40379 139.59184) 126.36129 153.8943 80 80
    withTexture (uniformTexture bellyColor)
      . withClipping (fill $ roundedRectangle (V2 16.163944 215.98409) 212.15131 99.693665 55 55)
      . fill
      $ roundedRectangle (V2 58.40379 139.59184) 126.36129 153.8943 80 80


drawHead :: px -> px -> Drawing px ()
drawHead frontColor backColor =
  do
    withTexture (uniformTexture backColor)
      . fill
      $ roundedRectangle (V2 20.827299 50.318928) 201.86699 116.59785 60 60
    withTexture (uniformTexture frontColor)
      . fill
      $ roundedRectangle (V2 20.827299 34.955734) 201.86699 116.59785 60 60


drawEyes :: px -> px -> Drawing px ()
drawEyes eyeColor pupilColor =
  do
    withTexture (uniformTexture eyeColor)
      $ mapM_ fill
      [
        circle (V2 75.04925 101.45342) 9.72327
      , circle (V2 (2*122.53-75.04925) 101.45342) 9.72327
      ]
    withTexture (uniformTexture pupilColor)
      $ mapM_ fill
      [
        circle (V2 70.61203 97.36941) 3.64652
      , circle (V2 (2*122.53-70.61203) 97.36941) 3.64652
      ]


drawEars :: px -> px -> Drawing px ()
drawEars frontColor backColor =
  do
    withTexture (uniformTexture backColor)
      $ mapM_ (fill . cubicBezierFromPath)
        [
          [
            V2 0 0.03337
          , V2 34.875935 0.42684743
          , V2 69.101494 15.066973
          , V2 85.434346 34.902808
          , V2 69.497156 38.440122
          , V2 51.422301 45.66022
          , V2 37.471204 58.134955
          , V2 42.774045 32.747291
          , V2 31.658189 11.934829
          , V2 0 0.03337
          ]
        , [
            V2 (2*122.53) 0.03337
          , V2 (2*122.53-34.875935) 0.42684743
          , V2 (2*122.53-69.101494) 15.066973
          , V2 (2*122.53-85.434346) 34.902808
          , V2 (2*122.53-69.497156) 38.440122
          , V2 (2*122.53-51.422301) 45.66022
          , V2 (2*122.53-37.471204) 58.134955
          , V2 (2*122.53-42.774045) 32.747291
          , V2 (2*122.53-31.658189) 11.934829
          , V2 (2*122.53) 0.03337
          ]
        ]
    withTexture (uniformTexture frontColor)
      $ mapM_ (fill . cubicBezierFromPath)
      [
        [       
          V2 0 0.03337
        , V2 50.861558 15.800834
        , V2 38.191333 57.31195
        , V2 37.471204 58.134955
        , V2 33.553602 63.778565
        , V2 30.631682 69.593209
        , V2 27.302137 75.122339
        , V2 14.99146 52.777337
        , V2 18.687946 21.667265
        , V2 0 0.03337
        ]
      , [       
          V2 (2*122.53) 0.03337
        , V2 (2*122.53-50.861558) 15.800834
        , V2 (2*122.53-38.191333) 57.31195
        , V2 (2*122.53-37.471204) 58.134955
        , V2 (2*122.53-33.553602) 63.778565
        , V2 (2*122.53-30.631682) 69.593209
        , V2 (2*122.53-27.302137) 75.122339
        , V2 (2*122.53-14.99146) 52.777337
        , V2 (2*122.53-18.687946) 21.667265
        , V2 (2*122.53) 0.03337
        ]
      ]


drawNose :: px -> px -> px -> px -> Drawing px ()
drawNose frontColor backColor centerColor nostrilColor =
  do
    withTexture (uniformTexture backColor)
      . fill
      $ roundedRectangle (V2 86.188965 111.72396) 71.934334 39.709103 15 15
    withTexture (uniformTexture frontColor)
      . fill
      $ roundedRectangle (V2 86.188965 107.60213) 71.934334 39.709103 15 15
    withTexture (uniformTexture centerColor)
      . stroke 1 JoinRound (CapStraight 0, CapStraight 0)
      $ line (V2 122.53 107.60213) (V2 122.53 (107.60213+39.709103))
    withTexture (uniformTexture nostrilColor)
      $ do
        fill
          $ roundedRectangle (V2 101.65501 117.96757) 10.00565 16.56616 3.8053 3.8053
        fill
          $ roundedRectangle (V2 (2 * 122.53-101.65501-10.00565) 117.96757) 10.00565 16.56616 3.8053 3.8053
