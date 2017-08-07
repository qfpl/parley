{-# LANGUAGE OverloadedStrings #-}

module Parley.DB where

import           Data.Either                        (rights)
import           Data.Monoid                        ((<>))
import           Data.Text                          (Text)
import           Data.Time.Clock                    (getCurrentTime)
import           Database.SQLite.Simple             (Connection,
                                                     NamedParam ((:=)),
                                                     Only (..), Query (..),
                                                     close, executeNamed,
                                                     execute_, open, query,
                                                     query_)
import           Database.SQLite.SimpleErrors       (runDBAction)
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Parley.Types                       (Comment,
                                                     CommentText (getComment),
                                                     Error (SQLiteError),
                                                     Topic (getTopic),
                                                     fromDbComment)

-- | Create the database and table as necessary
initDB :: FilePath
       -> Text
       -> IO (Either SQLiteResponse Connection)
initDB dbPath tbl = runDBAction $ do
  let createQ =
        Query ("CREATE TABLE IF NOT EXISTS " <> tbl
            <> " (id INTEGER PRIMARY KEY, topic TEXT,"
            <> "  comment TEXT, time INTEGER)")
  conn <- open dbPath
  execute_ conn createQ
  pure conn

closeDB :: Connection -> IO ()
closeDB = close

getComments :: Connection
            -> Topic
            -> IO (Either Error [Comment])
getComments conn t = do
  let q =  "SELECT id, topic, comment, time "
        <> "FROM comments WHERE topic = ?"
      p = Only (getTopic t)
  result <- runDBAction (query conn q p)
  case result of
    Left e -> (pure . Left . SQLiteError) e
    Right cs ->
      (pure . Right . rights . fmap fromDbComment) cs

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
toError ea =
  case ea of
    Left e  -> Left (SQLiteError e)
    Right a -> Right a
