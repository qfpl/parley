{-# LANGUAGE OverloadedStrings #-}
module Parley.DB where

import           Data.Text                          (Text)
import           Database.SQLite.Simple             (Connection,
                                                     NamedParam ((:=)),
                                                     Only (..), close,
                                                     executeNamed, execute_,
                                                     open, query)
import           Database.SQLite.SimpleErrors       (runDBAction)
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Parley.Types                       (Comment (..))

-- | Create the database and table as necessary
initDB :: FilePath -> Text -> IO (Either SQLiteResponse Connection)
initDB dbPath _tbl = runDBAction $ do
  conn <- open dbPath
  execute_ conn "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT)"
  execute_ conn "CREATE INDEX IF NOT EXISTS idx_comments_topic ON comments (topic)"
  pure conn

closeDB :: Connection -> IO ()
closeDB = close

getComments :: Connection -> Text -> IO (Either SQLiteResponse [Comment])
getComments conn topic =
  let q = "SELECT id, topic, comment FROM comments WHERE topic = ?"
   in runDBAction $ query conn q (Only topic)

addCommentToTopic :: Connection -> Text -> Text -> IO (Either SQLiteResponse ())
addCommentToTopic conn topic comment =
  let q      = "INSERT INTO comments (topic, comment) VALUES (:topic, :comment)"
      params = [":topic" := topic, ":comment" := comment]
   in runDBAction $ executeNamed conn q params
