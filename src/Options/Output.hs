module Options.Output where
data PrintOptions = PrintOptions
  { invert :: Bool
  , length :: Double
  , width :: Double
  , height :: Double
  , output :: String
  , filename :: String
  }
