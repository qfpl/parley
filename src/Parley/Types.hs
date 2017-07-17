{-# LANGUAGE OverloadedStrings #-}

module Parley.Types where

import           Control.Applicative                (liftA3)

import           Data.Aeson                         (ToJSON, object, pairs,
                                                     toEncoding, toJSON, (.=))
import qualified Data.ByteString.Lazy               as LBS
import           Data.Monoid                        ((<>))
import           Data.Text                          (Text)
import           Data.Text.Encoding                 (decodeUtf8)
import           Database.SQLite.Simple             (FromRow (fromRow), field)
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

data ContentType = PlainText
                 | JSON

instance Show ContentType where
  show PlainText = "text/plain"
  show JSON      = "text/json"

data ParleyRequest = AddRequest Add
                   | ViewRequest Text

data Add = Add { addTopic   :: Text
               , addComment :: Text
               }

mkAddRequest :: Text -> LBS.ByteString -> Either Error ParleyRequest
mkAddRequest "" _ = Left NoTopicInRequest
mkAddRequest _ "" = Left NoCommentText
mkAddRequest t b =
  let body = decodeUtf8 . LBS.toStrict $ b
   in pure . AddRequest $ Add t body

data Error = NoTopicInRequest
           | UnknownRoute
           | NoCommentText
           | SQLiteError SQLiteResponse

data Comment = Comment { commentId      :: Integer
                       , commentTopic   :: Text
                       , commentComment :: Text
                       }
               deriving Show

instance FromRow Comment where
  fromRow = liftA3 Comment field field field

instance ToJSON Comment where
  toJSON (Comment id' topic comment) =
    object ["id" .= id', "topic" .= topic, "comment" .= comment]
  toEncoding (Comment id' topic comment) =
    pairs ("id" .= id' <> "topic" .= topic <> "comment" .= comment)
