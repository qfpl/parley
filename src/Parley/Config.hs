{-# LANGUAGE RecordWildCards #-}

-- | Parley's configuration - based on the partial options monoid.
-- See https://medium.com/@jonathangfischoff/the-partial-options-monoid-pattern-31914a71fc67
module Parley.Config where

import           Data.Int    (Int16)
import           Data.Monoid (Last (..), (<>))

newtype Port = Port Int16

defaultConfig :: PartialConfig
defaultConfig = PartialConfig { pcPort = pure (Port 8080)
                              , pcDBPath = pure "parley.sqlite"
                              }

data PartialConfig = PartialConfig { pcPort   :: Last Port
                                   , pcDBPath :: Last FilePath
                                   }

data Config = Config { port   :: Port
                     , dbPath :: FilePath
                     }

instance Monoid PartialConfig where
  mempty = PartialConfig mempty mempty
  mappend a b = mempty { pcPort = pcPort a <> pcPort b
                       , pcDBPath = pcDBPath a <> pcDBPath b
                       }

makeConfig :: PartialConfig -> Either String Config
makeConfig PartialConfig {..} = do
  port <- lastToEither "Missing port" pcPort
  dbPath <- lastToEither "Missing database path" pcDBPath
  pure Config {..}

lastToEither :: e -> Last a -> Either e a
lastToEither e (Last m) = maybe (Left e) Right m
