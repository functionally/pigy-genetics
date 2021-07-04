module Pigy.Image (
  test
) where


import Codec.Picture( PixelRGBA8( .. ), writePng )
import Graphics.Rasterific
import Graphics.Rasterific.Texture


test :: IO ()
test =
  let
    white      = PixelRGBA8 0xFF 0xFF 0xFF 0xFF
    black      = PixelRGBA8 0x00 0x00 0x00 0xFF
    img =
      renderDrawing 245 287 white
        $ do
          drawBottomBack $ PixelRGBA8 0xff 0x57 0xa7 0xff
          drawTorso      $ PixelRGBA8 0xff 0x57 0xa7 0xff
          drawHeadBack   $ PixelRGBA8 0xff 0x41 0x9c 0xff
          drawHeadFront  $ PixelRGBA8 0xff 0x70 0xb5 0xff
          drawNoseBack   $ PixelRGBA8 0xff 0x31 0x94 0xff
          drawNoseFront  $ PixelRGBA8 0xff 0xad 0xd4 0xff
          drawNoseCenter $ PixelRGBA8 0xff 0x70 0xb5 0xff
          drawNostrils   black
          drawBelly      $ PixelRGBA8 0xff 0x85 0xc0 0xff
          drawEyes       black
          drawPupils     white
          drawEarsBack   $ PixelRGBA8 0xff 0x57 0xa7 0xff
          drawEarsFront  $ PixelRGBA8 0xff 0x8a 0xc2 0xff
  in
    writePng "yourimage.png" img


drawEarsFront color =
  withTexture (uniformTexture color)
    $ do
      fill $ cubicBezierFromPath
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
      fill $ cubicBezierFromPath
        [       
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


drawEarsBack color =
  withTexture (uniformTexture color)
    $ do
      fill $ cubicBezierFromPath
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
      fill $ cubicBezierFromPath
        [
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


drawPupils color =
  do
    withTexture (uniformTexture color)
      $ fill $ circle (V2 70.61203 97.36941) 3.64652
    withTexture (uniformTexture color)
      $ fill $ circle (V2 (2*122.53-70.61203) 97.36941) 3.64652


drawEyes color =
  do
    withTexture (uniformTexture color)
      $ fill $ circle (V2 75.04925 101.45342) 9.72327
    withTexture (uniformTexture color)
      $ fill $ circle (V2 (2*122.53-75.04925) 101.45342) 9.72327


drawNostrils color =
  do
    withTexture (uniformTexture color)
      $ fill $ roundedRectangle (V2 101.65501 117.96757) 10.00565 16.56616 3.8053 3.8053
    withTexture (uniformTexture color)
      $ fill $ roundedRectangle (V2 (2 * 122.53-101.65501-10.00565) 117.96757) 10.00565 16.56616 3.8053 3.8053


drawNoseCenter color =
  withTexture (uniformTexture color)
    $ stroke 1 JoinRound (CapStraight 0, CapStraight 0) $ line (V2 122.53 107.60213) (V2 122.53 (107.60213+39.709103))


drawNoseFront color =
  withTexture (uniformTexture color)
    $ fill $ roundedRectangle (V2 86.188965 107.60213) 71.934334 39.709103 15 15


drawNoseBack color =
  withTexture (uniformTexture color)
    $ fill $ roundedRectangle (V2 86.188965 111.72396) 71.934334 39.709103 15 15


drawHeadFront color =
  withTexture (uniformTexture color)
    $ fill $ roundedRectangle (V2 20.827299 34.955734) 201.86699 116.59785 60 60


drawHeadBack color =
  withTexture (uniformTexture color)
    $ fill $ roundedRectangle (V2 20.827299 50.318928) 201.86699 116.59785 60 60


drawBelly color =
  withTexture (uniformTexture color)
    $ withClipping (fill $ roundedRectangle (V2 16.163944 215.98409) 212.15131 99.693665 55 55)
    $ fill $ roundedRectangle (V2 58.40379 139.59184) 126.36129 153.8943 80 80


drawTorso color =
  withTexture (uniformTexture color)
    $ fill $ roundedRectangle (V2 58.40379 139.59184) 126.36129 153.8943 80 80


drawBottomBack color =
  withTexture (uniformTexture color)
    . fill
    $ roundedRectangle (V2 16.163944 215.98409) 212.15131 99.693665 55 55

