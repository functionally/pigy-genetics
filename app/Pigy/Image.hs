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
    pinkLight  = PixelRGBA8 0xFF 0xB0 0xD1 0xFF
    pinkMedium = PixelRGBA8 0xFC 0x82 0xB1 0xFF
    pinkDark   = PixelRGBA8 0xE9 0x56 0x8D 0xFF
    pink0 = PixelRGBA8 0xff 0x57 0xa7 0xff
    pink1 = PixelRGBA8 0xff 0x57 0xa7 0xff
    pink2 = PixelRGBA8 0xff 0x41 0x9c 0xff
    pink3 = PixelRGBA8 0xff 0x70 0xb5 0xff
    pink4 = PixelRGBA8 0xff 0x31 0x94 0xff
    pink5 = PixelRGBA8 0xff 0xad 0xd4 0xff
    pink6 = PixelRGBA8 0xff 0x85 0xc0 0xff
    pink7 = PixelRGBA8 0xff 0x57 0xa7 0xff
    pink8 = PixelRGBA8 0xff 0x8a 0xc2 0xff
    w = 2 * 122.53
    h = 287
    img =
      renderDrawing 245 287 white
        $ do
          withTexture (uniformTexture pink0)
            $ fill $ roundedRectangle (V2 16.163944 215.98409) 212.15131 99.693665 55 55
          withTexture (uniformTexture pink1)
            $ fill $ roundedRectangle (V2 58.40379 139.59184) 126.36129 153.8943 80 80
          withTexture (uniformTexture pink2)
            $ fill $ roundedRectangle (V2 20.827299 50.318928) 201.86699 116.59785 60 60
          withTexture (uniformTexture pink3)
            $ fill $ roundedRectangle (V2 20.827299 34.955734) 201.86699 116.59785 60 60
          withTexture (uniformTexture pink4)
            $ fill $ roundedRectangle (V2 86.188965 111.72396) 71.934334 39.709103 15 15
          withTexture (uniformTexture pink5)
            $ fill $ roundedRectangle (V2 86.188965 107.60213) 71.934334 39.709103 15 15
          withTexture (uniformTexture black)
            $ fill $ roundedRectangle (V2 101.65501 117.96757) 10.00565 16.56616 3.8053 3.8053
          withTexture (uniformTexture black)
            $ fill $ roundedRectangle (V2 (w-101.65501-10.00565) 117.96757) 10.00565 16.56616 3.8053 3.8053
          withTexture (uniformTexture pink6)
            $ withClipping (fill $ roundedRectangle (V2 16.163944 215.98409) 212.15131 99.693665 55 55)
            $ fill $ roundedRectangle (V2 58.40379 139.59184) 126.36129 153.8943 80 80
          withTexture (uniformTexture pink3)
            $ stroke 1 JoinRound (CapStraight 0, CapStraight 0) $ line (V2 (w/2) 107.60213) (V2 (w/2) (107.60213+39.709103))
          withTexture (uniformTexture black)
            $ fill $ circle (V2 75.04925 101.45342) 9.72327
          withTexture (uniformTexture black)
            $ fill $ circle (V2 (w-75.04925) 101.45342) 9.72327
          withTexture (uniformTexture white)
            $ fill $ circle (V2 70.61203 97.36941) 3.64652
          withTexture (uniformTexture white)
            $ fill $ circle (V2 (w-70.61203) 97.36941) 3.64652
          withTexture (uniformTexture pink7)
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
                  V2 (w-0) 0.03337
                , V2 (w-34.875935) 0.42684743
                , V2 (w-69.101494) 15.066973
                , V2 (w-85.434346) 34.902808
                , V2 (w-69.497156) 38.440122
                , V2 (w-51.422301) 45.66022
                , V2 (w-37.471204) 58.134955
                , V2 (w-42.774045) 32.747291
                , V2 (w-31.658189) 11.934829
                , V2 (w-0) 0.03337
                ]
          withTexture (uniformTexture pink8)
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
                  V2 (w-0) 0.03337
                , V2 (w-50.861558) 15.800834
                , V2 (w-38.191333) 57.31195
                , V2 (w-37.471204) 58.134955
                , V2 (w-33.553602) 63.778565
                , V2 (w-30.631682) 69.593209
                , V2 (w-27.302137) 75.122339
                , V2 (w-14.99146) 52.777337
                , V2 (w-18.687946) 21.667265
                , V2 (w-0) 0.03337
                ]
  in
    writePng "yourimage.png" img
