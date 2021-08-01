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
-- | Drawing pig images.
--
-----------------------------------------------------------------------------


{-# LANGUAGE RecordWildCards       #-}


module Pigy.Image.Drawing (
-- * Drawing
  drawBody
, drawEars
, drawEyes
, drawHead
, drawNose
-- * Scaling
, withAspect
, withScale
-- * Colors
, skin
-- * Dimensions
, enlarge
, width
, height
) where


import Codec.Picture                       (PixelRGBA8(..))
import Data.Colour.RGBSpace.HSL            (hsl)
import Data.Colour.SRGB                    (RGB(..))
import Graphics.Rasterific                 (Cap(..), Drawing, Join(..), Texture, V2(..), circle, cubicBezierFromPath, fill, line, roundedRectangle, stroke, withClipping, withTexture, withTransformation)
import Graphics.Rasterific.Transformations (scale, translate)


-- | Image enlargment factor, relative to nominal dimensions.
enlarge :: Float
enlarge = 2


-- | Nominal image width.
width :: Float
width = 245


-- | Nominal image height.
height :: Float
height = 287


-- | Draw the body.
drawBody :: Float         -- ^ The scale for the body.
         -> Texture px    -- ^ The torso color.
         -> Texture px    -- ^ The belly color.
         -> Texture px    -- ^ The bottom color.
         -> Drawing px () -- ^ The drawing.
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


-- | Draw the head.
drawHead :: Texture px    -- ^ The unshadowed color.
         -> Texture px    -- ^ The shadowed color.
         -> Drawing px () -- ^ The drawing.
drawHead frontColor backColor =
  do
    withTexture backColor
      . fill
      $ roundedRectangle (V2 20.827299 50.318928) 201.86699 116.59785 60 60
    withTexture frontColor
      . fill
      $ roundedRectangle (V2 20.827299 34.955734) 201.86699 116.59785 60 60


-- | Draw the eyes.
drawEyes :: (Float, Float) -- ^ The pupil position in radial coordinates.
         -> (Float, Float) -- ^ The scaling of the eye.
         -> Texture px     -- ^ The eye color.
         -> Texture px     -- ^ The pupil color.
         -> Drawing px ()  -- ^ The drawing.
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


-- | Draw the ears.
drawEars :: (Float, Float) -- ^ The scaling of the ears.
         -> Texture px     -- ^ The unshadowed color.
         -> Texture px     -- ^ The shadowed color.
         -> Drawing px ()  -- ^ The drawing.
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


-- | Draw the nose.
drawNose :: Texture px    -- ^ The unshadowed color.
         -> Texture px    -- ^ The shadowed color.
         -> Texture px    -- ^ The centerline color.
         -> Texture px    -- ^ The nostril color.
         -> Drawing px () -- ^ The drawing.
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


-- | Scale a drawing according to an aspect ratio.
withAspect :: Float          -- ^ The aspect ratio.
           -> (Float, Float) -- ^ The center of the scaling.
           -> Drawing px ()  -- ^ The original drawing.
           -> Drawing px ()  -- ^ The scaled drawing.
withAspect ratio =
  withScale
    (
      minimum [1,     ratio]
    , minimum [1, 1 / ratio]
    )


-- | Scale a drawing.
withScale :: (Float, Float) -- ^ The scales.
          -> (Float, Float) -- ^ The center of the scaling.
          -> Drawing px ()  -- ^ The original drawing.
          -> Drawing px ()  -- ^ The scaled drawing.
withScale (sx, sy) (cx, cy) =
  withTransformation
    $  translate (V2    cx     cy )
    <> scale sx sy
    <> translate (V2 (- cx) (- cy))


-- | Compute the skin color.
skin :: Float      -- ^ The hue.
     -> Float      -- ^ The luminosity.
     -> PixelRGBA8 -- ^ The skin color.
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
