{-# LANGUAGE OverloadedStrings #-}
module Parley.DB where

import           Data.Text              (Text)
import           Database.SQLite.Simple (Connection, Only (..), close, execute_,
                                         open, query)

import           Parley.Types           (Comment (..))

-- | Create the database and table as necessary
initDB :: FilePath -> Text -> IO Connection
initDB dbPath _tbl = do
  conn <- open dbPath
  execute_ conn "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT)"
  execute_ conn "CREATE INDEX IF NOT EXISTS idx_comments_topic ON comments (topic)"
  pure conn

closeDB :: Connection -> IO ()
closeDB = close

getCommentsForTopic :: Connection -> Text -> IO [Comment]
getCommentsForTopic conn topic =
  query conn "SELECT id, topic, comment FROM comments WHERE topic = ?" (Only topic)
