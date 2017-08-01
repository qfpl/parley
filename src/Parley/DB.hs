{-# LANGUAGE OverloadedStrings #-}

module Parley.DB where

import           Data.Either                        (rights)
import           Data.Text                          (Text)
import           Data.Time.Clock                    (getCurrentTime)
import           Database.SQLite.Simple             (Connection,
                                                     NamedParam ((:=)),
                                                     Only (..), close,
                                                     executeNamed, execute_,
                                                     open, query, query_)
import           Database.SQLite.SimpleErrors       (runDBAction)
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Parley.Types                       (Comment,
                                                     CommentText (getComment),
                                                     Error (SQLiteError),
                                                     Topic (getTopic),
                                                     fromDbComment)

-- | Create the database and table as necessary
initDB :: FilePath -> Text -> IO (Either SQLiteResponse Connection)
initDB dbPath _tbl = runDBAction $ do
  conn <- open dbPath
  execute_ conn "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"
  execute_ conn "CREATE INDEX IF NOT EXISTS idx_comments_topic ON comments (topic)"
  pure conn

closeDB :: Connection -> IO ()
closeDB = close

getComments :: Connection -> Topic -> IO (Either Error [Comment])
getComments conn =
  let q = "SELECT id, topic, comment, time FROM comments WHERE topic = ?"
      convert = either (Left . SQLiteError) (Right . rights . fmap fromDbComment)
   in fmap convert . runDBAction . query conn q . Only . getTopic

addCommentToTopic :: Connection -> Topic -> CommentText -> IO (Either SQLiteResponse ())
addCommentToTopic conn t c = do
  now <- getCurrentTime
  let q      = "INSERT INTO comments (topic, comment, time) VALUES (:topic, :comment, :time)"
      params = [":topic" := getTopic t, ":comment" := getComment c, ":time" := now]
   in runDBAction $ executeNamed conn q params

getTopics :: Connection -> IO (Either Error [Text])
getTopics =
  fmap toError . runDBAction . fmap concat . flip query_ "SELECT DISTINCT(topic) FROM comments"

toError :: Either SQLiteResponse a -> Either Error a
toError = either (Left . SQLiteError) Right
