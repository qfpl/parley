{-# LANGUAGE OverloadedStrings #-}

module Parley.Types ( Comment
                    , ContentType (..)
                    , Error (..)
                    , ParleyRequest (..)
                    , CommentText (getComment)
                    , Topic (getTopic)
                    , mkAddRequest
                    , mkViewRequest
                    ) where

import           Control.Applicative                (liftA2)

import           Data.Aeson                         (ToJSON, object, pairs,
                                                     toEncoding, toJSON, (.=))
import qualified Data.ByteString.Lazy               as LBS
import           Data.Monoid                        ((<>))
import           Data.Text                          (Text)
import           Data.Text.Encoding                 (decodeUtf8)
import           Data.Time.Clock                    (UTCTime)
import           Database.SQLite.Simple             (FromRow (fromRow), field)
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

data ParleyRequest = AddRequest Topic CommentText
                   | ViewRequest Topic
                   | ListRequest

data Error = NoTopicInRequest
           | UnknownRoute
           | NoCommentText
           | SQLiteError SQLiteResponse

data Comment =
  Comment { _commentId      :: Integer
          , _commentTopic   :: Text
          , _commentComment :: Text
          , _commentTime    :: UTCTime
          }
          deriving Show

data ContentType = PlainText
                 | JSON

newtype Topic = Topic {getTopic :: Text} deriving (Eq, Show)

newtype CommentText = CommentText { getComment :: Text } deriving (Eq, Show)

mkAddRequest :: Text -> LBS.ByteString -> Either Error ParleyRequest
mkAddRequest "" _ = Left NoTopicInRequest
mkAddRequest _ "" = Left NoCommentText
mkAddRequest t b =
  liftA2 AddRequest (mkTopic t) . mkCommentText . decodeUtf8 $ LBS.toStrict b

mkViewRequest :: Text -> Either Error ParleyRequest
mkViewRequest "" = Left NoTopicInRequest
mkViewRequest t  = ViewRequest <$> mkTopic t

mkTopic :: Text -> Either Error Topic
mkTopic "" = Left NoTopicInRequest
mkTopic t  = pure $ Topic t

mkCommentText :: Text -> Either Error CommentText
mkCommentText "" = Left NoCommentText
mkCommentText t  = pure $ CommentText t

instance FromRow Comment where
  fromRow = Comment <$> field <*> field <*> field <*> field

instance ToJSON Comment where
  toJSON (Comment id' topic comment time) =
    object ["id" .= id', "topic" .= topic, "comment" .= comment, "time" .= time]
  toEncoding (Comment id' topic comment time) =
    pairs ("id" .= id' <> "topic" .= topic <> "comment" .= comment <> "time" .= time)

instance Show ContentType where
  show PlainText = "text/plain"
  show JSON      = "text/json"
