module Location.Domain.Configuration.Config
  ( LocationConfig (..),
    config,
  )
where

data LocationConfig = LocationConfig
  { outPath :: String,
    prefix :: String,
    extension :: String
  }

config :: String -> String -> String -> LocationConfig
config = LocationConfig
