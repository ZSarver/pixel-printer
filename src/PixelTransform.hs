{- |
Module      : PixelTransform
Description : Transforms image data into geometry
Copyright   : (c) Zachary Sarver, 2021
License     : GPL-3
Maintainer  : Zachary.Sarver@gmail.com

Transforms RGB8 image data into geometry, where each pixel is transformed into a
square prism, where length and width are fixed and heigh is determined by pixel
brightness or hue.
-}
module PixelTransform
    ( geomBrightnessImage
    , geomHueImage
    ) where

import Codec.Picture ( Image(..)
                     , PixelRGB8(..)
                     , imageIPixels
                     , pixelAt
                     )
import Geometry (Geometry(..))
import Control.Lens.Traversal(mapAccumLOf)
import Data.Fixed (mod')

import qualified Options.Output as PO

type Degrees = Double
type PixelPropertySelector = (PixelRGB8 -> Bool -> Double)

logScalePixel :: Double -> Double -> Double
logScalePixel p height = 1 + logBase 2 pp - logBase 2 255.0 + logBase 2 height
  where pp = max p 2

brightness :: PixelRGB8 -> Bool -> Double
brightness (PixelRGB8 r g b) invert = if invert
          then (fromIntegral r + fromIntegral g + fromIntegral b) / 3.0
          else 255.0 - (fromIntegral r + fromIntegral g + fromIntegral b) / 3.0

hue :: PixelRGB8 -> Bool -> Degrees
hue (PixelRGB8 r g b) invert
  | chroma == 0.0 = 0.0
  | value == r' = invertHue $ mod' (60.0 * (g' - b') / chroma) 360.0
  | value == g' = invertHue $ mod' (60.0 * (2.0 + (b' - r') / chroma )) 360.0
  | value == b' = invertHue $ mod' (60.0 * (4.0 + (r' - g') / chroma )) 360.0
  | otherwise = 0.0
  where
    r' = (fromIntegral r) / 255.0 :: Double
    g' = (fromIntegral g) / 255.0 :: Double
    b' = (fromIntegral b) / 255.0 :: Double
    value = foldl max 0.0 [r', g', b']
    chroma = value - (foldl min 2.0 [r', g', b'])
    invertHue :: Double -> Double
    invertHue h = if invert then mod' (h + 180.0) 360.0 else h

geomPixel :: Int
  -> Int
  -> (Int, Int, PixelRGB8)
  -> PixelRGB8
  -> PO.PrintOptions
  -> PixelPropertySelector
  -> Geometry
geomPixel iw il (x,y,p) transparent options selector = if p == transparent
  then Empty
  else Translate
       (1+fromIntegral x*pixelWidth)
       (1+fromIntegral y*pixelLength)
       0
       (Cube pixelWidth pixelLength (logScalePixel (selector p (PO.invert options)) pixelHeight))
  where pixelLength = PO.length options / fromIntegral il
        pixelWidth = PO.width options / fromIntegral iw
        pixelHeight = 2 ** PO.height options -- we want user input to scale linearly, so we exponentiate by 2 to offset taking log 2 later

-- | Converts an 8-bit RGB image to a list of Cubes, each cube having fixed
-- width and depth with height varying according to pixel properties
geomImage :: PixelPropertySelector -> PO.PrintOptions -> Image PixelRGB8 ->  [Geometry]
geomImage selector options image = fst $ mapAccumLOf imageIPixels (\acc (x,y,px) -> (geomPixel (imageWidth image) (imageHeight image) (x,y,px) transparent options selector : acc, px)) [] image
  where transparent = pixelAt image 0 0

geomBrightnessImage :: PO.PrintOptions -> Image PixelRGB8 -> [Geometry]
geomBrightnessImage = geomImage brightness

geomHueImage :: PO.PrintOptions -> Image PixelRGB8 -> [Geometry]
geomHueImage = geomImage hue
