module Main where

import Codec.Picture ( Image(..)
                     , PixelRGB8(..)
                     , readImage
                     , convertRGB8
                     )
import System.Environment (getArgs)

import PixelTransform (geomImage)
import Scad (scadify)

main :: IO ()
main = do
  args <- getArgs
  let filename = head args
  eitherImage <- readImage filename
  either (putStrLn) (putStrLn . getScadCode . convertRGB8) eitherImage

getScadCode :: Image PixelRGB8 -> String
getScadCode = scadify . geomImage
