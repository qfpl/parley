-- | Parley's configuration - based on the partial options monoid.
-- See https://medium.com/@jonathangfischoff/the-partial-options-monoid-pattern-31914a71fc67
module Parley.Config where

import           Data.Int            (Int16)
import           Data.Monoid         (Last (..), (<>))
import           Options.Applicative (Parser, ParserInfo, auto, execParser,
                                      fullDesc, header, help, helper, info,
                                      long, metavar, option, optional, progDesc,
                                      short, strOption)

data ConfigError = MissingPort
                 | MissingDbPath
                 deriving Show

newtype Port = Port { unPort :: Int16 }
               deriving Show

instance Read Port where
  readsPrec =
    let portIt (n, s) = (Port n, s)
    in fmap (fmap portIt) . readsPrec

defaultConfig :: PartialConfig
defaultConfig =
  PartialConfig { pcPort = pure (Port 8080)
                , pcDBPath = pure "parley.sqlite"
                }

data PartialConfig =
  PartialConfig { pcPort   :: Last Port
                , pcDBPath :: Last FilePath
                }

data Config =
  Config { port   :: Port
         , dbPath :: FilePath
         }

instance Monoid PartialConfig where
  mempty = PartialConfig mempty mempty
  mappend a b = mempty { pcPort = pcPort a <> pcPort b
                       , pcDBPath = pcDBPath a <> pcDBPath b
                       }

makeConfig :: PartialConfig -> Either ConfigError Config
makeConfig pc = do
  let lastToEither e (Last Nothing) = Left e
      lastToEither _ (Last (Just v)) = Right v
  port' <- lastToEither MissingPort (pcPort pc)
  dbPath' <- lastToEither MissingDbPath (pcDBPath pc)
  pure Config {port = port', dbPath = dbPath'}

parseOptions :: FilePath -> IO (Either ConfigError Config)
parseOptions configFilePath = do
  fileConfig <- parseConfigFile configFilePath
  commandLineConfig <- parseCommandLine
  pure (makeConfig (defaultConfig <> fileConfig <> commandLineConfig))

-- TODO: implement config file parsing
parseConfigFile :: FilePath -> IO PartialConfig
parseConfigFile = const (pure mempty)

parseCommandLine :: IO PartialConfig
parseCommandLine = execParser commandLineParser

commandLineParser :: ParserInfo PartialConfig
commandLineParser =
  let mods =  fullDesc
           <> progDesc "Manage comments for a web blog"
           <> header "parley - simple comment management"
   in info (helper <*> partialConfigParser) mods

partialConfigParser :: Parser PartialConfig
partialConfigParser =
  PartialConfig <$> portParser <*> dbParser

portParser :: Parser (Last Port)
portParser =
  let portHelp = help "TCP port to accept requests on"
      mods = long "port" <> short 'p' <> metavar "PORT" <> portHelp
   in Last <$> optional (option auto mods)

dbParser :: Parser (Last FilePath)
dbParser =
  let dbHelp = help "Path to sqlite database"
      mods = long "database" <> short 'd' <> metavar "SQLITE_FILE" <> dbHelp
   in Last <$> optional (strOption mods)
