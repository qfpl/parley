{-# LANGUAGE OverloadedStrings #-}

module Parley.DB ( ParleyDb
                 , initDB
                 , closeDB
                 , getComments
                 , addCommentToTopic
                 , getTopics
                 ) where

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
                                                     Table (..),
                                                     Topic (getTopic),
                                                     fromDbComment, mkTopic)

data ParleyDb = ParleyDb Connection Table

-- | Create the database and table as necessary
initDB :: FilePath
       -> Text
       -> IO (Either SQLiteResponse ParleyDb)
initDB dbPath tbl = runDBAction $ do
  let createQ =
        Query ("CREATE TABLE IF NOT EXISTS " <> tbl
            <> " (id INTEGER PRIMARY KEY, topic TEXT,"
            <> "  comment TEXT, time INTEGER)")
  conn <- open dbPath
  execute_ conn createQ
  pure (ParleyDb conn (Table tbl))

closeDB :: ParleyDb -> IO ()
closeDB (ParleyDb conn _) = close conn

getComments :: ParleyDb
            -> Topic
            -> IO (Either Error [Comment])
getComments (ParleyDb conn _) t =
  let q =  "SELECT id, topic, comment, time "
        <> "FROM comments WHERE topic = ?"
      p = Only (getTopic t)
      result = query conn q p
   in dbToParley fromDbComment result

addCommentToTopic :: ParleyDb
                  -> Topic
                  -> CommentText
                  -> IO (Either Error ())
addCommentToTopic (ParleyDb conn _) t c = do
  now <- getCurrentTime
  let q      = "INSERT INTO comments (topic, comment, time) VALUES (:topic, :comment, :time)"
      params = [":topic" := getTopic t, ":comment" := getComment c, ":time" := now]
  result <- runDBAction (executeNamed conn q params)
  case result of
    Left e -> pure (Left (SQLiteError e))
    Right a -> pure (Right a)

getTopics :: ParleyDb -> IO (Either Error [Topic])
getTopics (ParleyDb conn (Table t)) =
  let q = Query ("SELECT DISTINCT(topic) FROM " <> t)
      result = query_ conn q
   in dbToParley mkTopic (fmap concat result)

dbToParley :: (a -> Either Error b)
           -> IO [a]
           -> IO (Either Error [b])
dbToParley f a = do
  result <- runDBAction a
  case result of
    Left e   -> (pure . Left . SQLiteError) e
    Right as -> (pure . Right . rights . fmap f) as
