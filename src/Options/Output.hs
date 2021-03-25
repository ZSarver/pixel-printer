module Options.Output (PrintOptions(..)) where
data PrintOptions = PrintOptions
  { invert :: Bool
  , length :: Double
  , width :: Double
  , height :: Double
  , filename :: String}
