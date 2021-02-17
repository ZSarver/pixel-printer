module Main where

import Codec.Picture ( Image(..)
                     , PixelRGB8(..)
                     , readImage
                     , convertRGB8
                     )
import Data.Semigroup((<>))
import Options.Applicative
import System.Environment (getArgs)

import PixelTransform (geomImage)
import Scad (scadify)

data PrintOptions = PrintOptions
  { invert :: Bool
  , filename :: String}

printOptions :: Parser PrintOptions
printOptions = PrintOptions
               <$> switch
               ( long "invert"
               <> short 'i'
               <> help "Inverts the height of each pixel, so that brighter pixels are taller and shorter are longer.")
               <*> strArgument (metavar "FILE")

printOptionsInfo :: ParserInfo PrintOptions
printOptionsInfo = info (printOptions <**> helper)
                  ( fullDesc
                  <> progDesc "Convert FILE to 3D openSCAD code")

main :: IO ()
main = do
  options <- execParser printOptionsInfo
  eitherImage <- readImage $ filename options
  either (putStrLn) (putStrLn . (getScadCode $ invert options) . convertRGB8) eitherImage

getScadCode :: Bool -> Image PixelRGB8 -> String
getScadCode b = scadify . (geomImage b)
