module Parley.Config where

import Data.Int (Int16)
import Data.Monoid (Last (..), (<>))

newtype Port = Port Int16

defaultConfig :: PartialConfig
defaultConfig = PartialConfig { pcPort = pure (Port 8080)
                              , pcDBPath = pure "parley.sqlite"
                              }

data PartialConfig = PartialConfig { pcPort :: Last Port
                                   , pcDBPath :: Last FilePath
                                   }

data Config = Config { port :: Port
                     , dbPath :: FilePath
                     }

instance Monoid PartialConfig where
  mempty = PartialConfig mempty mempty
  mappend a b = mempty { pcPort = pcPort a <> pcPort b
                       , pcDBPath = pcDBPath a <> pcDBPath b
                       }
