{-# LANGUAGE RecordWildCards #-}

-- | Parley's configuration - based on the partial options monoid.
-- See https://medium.com/@jonathangfischoff/the-partial-options-monoid-pattern-31914a71fc67
module Parley.Config where

import           Data.Foldable       (fold)
import           Data.Int            (Int16)
import           Data.Monoid         (Last (..), (<>))
import           Options.Applicative (Parser, ParserInfo, auto, execParser,
                                      fullDesc, header, help, helper, info,
                                      long, metavar, option, optional, progDesc,
                                      short, (<**>))

newtype Port = Port { unPort :: Int16 }
               deriving Show

instance Read Port where
  readsPrec = fmap (fmap (\(n, s) -> (Port n, s))) . readsPrec

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

parseOptions :: FilePath -> IO (Either String Config)
parseOptions configFilePath =
  let configs = sequenceA [ parseConfigFile configFilePath
                          , parseCommandLine
                          ]
   in fmap (makeConfig . fold . (defaultConfig :)) configs

-- TODO: implement config file parsing
parseConfigFile :: FilePath -> IO PartialConfig
parseConfigFile = const (pure mempty)

parseCommandLine :: IO PartialConfig
parseCommandLine = execParser commandLineParser

commandLineParser :: ParserInfo PartialConfig
commandLineParser = info (partialConfigParser <**> helper)
                         (  fullDesc
                         <> progDesc "Manage comments for a web blog"
                         <> header "parley - simple comment management"
                         )

partialConfigParser :: Parser PartialConfig
partialConfigParser =
  PartialConfig <$> portParser <*> dbParser

portParser :: Parser (Last Port)
portParser =
  let portHelp = help "TCP port to accept requests on"
   in Last <$> (optional $ option auto (long "port" <> short 'p' <> metavar "PORT" <> portHelp))

dbParser :: Parser (Last FilePath)
dbParser =
  let dbHelp = help "Path to sqlite database"
   in Last <$> (optional $ option auto (long "database" <> short 'd' <> metavar "SQLITE_FILE" <> dbHelp))
