module PixelTransform
    ( geomImage
    ) where

import Codec.Picture ( imageIPixels
                     , pixelMap
                     , DynamicImage(..)
                     , Image(..)
                     , PixelRGB8(..)
                     , Pixel8(..)
                     )
import Geometry (Geometry(..))
import Control.Lens.Traversal(mapAccumLOf)

pixelWidth :: Int
pixelWidth = 5

pixelLength :: Int
pixelLength = 5

pixelHeight :: Int
pixelHeight = 15

convertY8 :: Image PixelRGB8 -> Image Pixel8
convertY8 = pixelMap (\(PixelRGB8 r g b) -> (r + g + b) `div` 3)

scalePixel :: Pixel8 -> Int -> Int
scalePixel p height = if p == 0
                      then 1
                      else round $ (fromIntegral height) / 255.0 + 1

geomPixel :: (Int, Int, Pixel8) -> Geometry
geomPixel (x,y,p) = Transform ( 1+x*pixelWidth ) ( 1+y*pixelLength ) 0 (Cube pixelWidth pixelLength (scalePixel p pixelHeight))

geomImage8 :: Image Pixel8 -> [Geometry]
geomImage8 image = fst $ mapAccumLOf imageIPixels (\acc (x,y,px) -> (geomPixel (x,y,px) : acc, id px)) [] image

geomImage :: Image PixelRGB8 -> [Geometry]
geomImage = geomImage8 . convertY8
