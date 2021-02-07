module PixelTransform
    ( geomImage
    ) where

import Codec.Picture ( DynamicImage(..)
                     , Image(..)
                     , PixelRGB8(..)
                     , Pixel8(..)
                     , imageIPixels
                     , pixelMap
                     , pixelAt
                     )
import Geometry (Geometry(..))
import Control.Lens.Traversal(mapAccumLOf)

pixelWidth :: Int
pixelWidth = 5

pixelLength :: Int
pixelLength = 5

pixelHeight :: Int
pixelHeight = 100

scalePixel :: Pixel8 -> Int -> Int
scalePixel p height = round $ (fromIntegral p) / 255.0 * (fromIntegral height) + 1

logScalePixel :: Pixel8 -> Int -> Int
logScalePixel p height = 1 + ( round $ logBase 2 (fromIntegral pp) - logBase 2 255.0 + logBase 2 (fromIntegral height) )
  where pp = max p 2

geomPixel :: (Int, Int, PixelRGB8) -> PixelRGB8 -> Geometry
geomPixel (x,y,p) transparent = if p == transparent
  then Empty
  else Transform ( 1+x*pixelWidth ) ( 1+y*pixelLength ) 0 (Cube pixelWidth pixelLength (logScalePixel (monochrome p) pixelHeight))
  where monochrome (PixelRGB8 r g b) = (r + g + b) `div` 3

geomImage :: Image PixelRGB8 -> [Geometry]
geomImage image = fst $ mapAccumLOf imageIPixels (\acc (x,y,px) -> (geomPixel (x,y,px) transparent : acc, id px)) [] image
  where transparent = pixelAt image 0 0
