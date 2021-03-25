module Main where

import Codec.Picture ( Image(..)
                     , PixelRGB8(..)
                     , readImage
                     , convertRGB8
                     )
import Options.Applicative

import Options.Output (PrintOptions(..))
import PixelTransform (geomImage)
import Scad (scadify)

printOptions :: Parser PrintOptions
printOptions = PrintOptions
               <$> switch
                  ( long "invert"
                  <> short 'i'
                  <> help "Inverts the height of each pixel, so that brighter pixels are taller and shorter are longer.")
               <*> option auto
                  ( long "length"
                  <> short 'l'
                  <> help "Sets the length of the output."
                  <> metavar "LEN"
                  <> value 100
                  <> showDefault)
               <*> option auto
                  ( long "width"
                  <> short 'w'
                  <> help "Sets the width of output."
                  <> metavar "WIDTH"
                  <> value 100
                  <> showDefault)
               <*> option auto
                  ( long "height"
                  <> short 'h'
                  <> help "Sets the maximum height of the output - pixels will be scaled to this height based on brightness."
                  <> metavar "HEIGHT"
                  <> value 10
                  <> showDefault
                  )
               <*> strArgument (metavar "FILE")

printOptionsInfo :: ParserInfo PrintOptions
printOptionsInfo = info (printOptions <**> helper)
                  ( fullDesc
                  <> progDesc "Convert FILE to 3D openSCAD code")

main :: IO ()
main = do
  options <- execParser printOptionsInfo
  eitherImage <- readImage $ filename options
  either putStrLn (putStrLn . getScadCode options . convertRGB8) eitherImage

getScadCode :: PrintOptions -> Image PixelRGB8 -> String
getScadCode po = scadify . geomImage po
