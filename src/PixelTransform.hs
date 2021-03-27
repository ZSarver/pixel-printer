{- |
Module      : PixelTransform
Description : Transforms image data into geometry
Copyright   : (c) Zachary Sarver, 2021
License     : GPL-3
Maintainer  : Zachary.Sarver@gmail.com

Transforms RGB8 image data into geometry, where each pixel is transformed into a
square prism, where length and width are fixed and heigh is determined by pixel
brightness.
-}
module PixelTransform
    ( geomImage
    ) where

import Codec.Picture ( Image(..)
                     , PixelRGB8(..)
                     , imageIPixels
                     , pixelAt
                     )
import Geometry (Geometry(..))
import Control.Lens.Traversal(mapAccumLOf)

import qualified Options.Output as PO

logScalePixel :: Double -> Double -> Double
logScalePixel p height = 1 + logBase 2 pp - logBase 2 255.0 + logBase 2 height
  where pp = max p 2

monochrome :: PixelRGB8 -> Bool -> Double
monochrome (PixelRGB8 r g b) invert = if invert
          then (fromIntegral r + fromIntegral g + fromIntegral b) / 3.0
          else 255.0 - (fromIntegral r + fromIntegral g + fromIntegral b) / 3.0

geomPixel :: Int -> Int -> (Int, Int, PixelRGB8) -> PixelRGB8 -> PO.PrintOptions -> Geometry
geomPixel iw il (x,y,p) transparent options = if p == transparent
  then Empty
  else Translate
       (1+fromIntegral x*pixelWidth)
       (1+fromIntegral y*pixelLength)
       0
       (Cube pixelWidth pixelLength (logScalePixel (monochrome p (PO.invert options)) pixelHeight))
  where pixelLength = PO.length options / fromIntegral il
        pixelWidth = PO.width options / fromIntegral iw
        pixelHeight = 2 ** PO.height options -- we want user input to scale linearly, so we exponentiate by 2 to offset taking log 2 later

-- | Converts an 8-bit RGB image to a list of Cubes, each cube having fixed
-- width and depth with height varying according to pixel brightness
geomImage :: PO.PrintOptions -> Image PixelRGB8 -> [Geometry]
geomImage options image = fst $ mapAccumLOf imageIPixels (\acc (x,y,px) -> (geomPixel (imageWidth image) (imageHeight image) (x,y,px) transparent options : acc, px)) [] image
  where transparent = pixelAt image 0 0
