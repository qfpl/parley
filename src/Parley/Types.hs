{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Parley.Types ( Comment
                    , ContentType (..)
                    , Error (..)
                    , ParleyRequest (..)
                    , CommentText (getComment)
                    , Topic (getTopic)
                    , fromDbComment
                    , mkAddRequest
                    , mkViewRequest
                    , render
                    ) where

import           Data.Aeson                         (ToJSON, object, pairs,
                                                     toEncoding, toJSON, (.=))
import           Data.ByteString.Char8              (ByteString)
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

data DbComment =
  DbComment { dbCommentId    :: Integer
            , dbCommentTopic :: Text
            , dbCommentBody  :: Text
            , dbCommentTime  :: UTCTime
            }
            deriving Show

data Comment = Comment CommentId
                       Topic
                       CommentText
                       UTCTime
               deriving Show

fromDbComment :: DbComment -> Either Error Comment
fromDbComment dbc =
  Comment     (CommentId (dbCommentId dbc))
          <$> mkTopic (dbCommentTopic dbc)
          <*> mkCommentText (dbCommentBody dbc)
          <*> pure (dbCommentTime dbc)

data ContentType = PlainText
                 | JSON

render :: ContentType -> ByteString
render PlainText = "text/plain"
render JSON      = "text/json"

newtype CommentId = CommentId Integer
                    deriving (Eq, Show, ToJSON)

newtype Topic = Topic {getTopic :: Text}
                deriving (Eq, Show, ToJSON)

newtype CommentText = CommentText {getComment :: Text}
                      deriving (Eq, Show, ToJSON)

mkAddRequest :: Text -> LBS.ByteString -> Either Error ParleyRequest
mkAddRequest "" _ = Left NoTopicInRequest
mkAddRequest _ "" = Left NoCommentText
mkAddRequest t b =
  let commentText = mkCommentText (decodeUtf8 (LBS.toStrict b))
   in AddRequest <$> (mkTopic t) <*> commentText

mkViewRequest :: Text -> Either Error ParleyRequest
mkViewRequest "" = Left NoTopicInRequest
mkViewRequest t  = ViewRequest <$> mkTopic t

mkTopic :: Text -> Either Error Topic
mkTopic "" = Left NoTopicInRequest
mkTopic t  = pure $ Topic t

mkCommentText :: Text -> Either Error CommentText
mkCommentText "" = Left NoCommentText
mkCommentText t  = pure $ CommentText t

instance FromRow DbComment where
  fromRow = DbComment <$> field <*> field <*> field <*> field

instance ToJSON Comment where
  toJSON (Comment id' topic comment time) =
    object ["id" .= id', "topic" .= topic, "comment" .= comment, "time" .= time]
  toEncoding (Comment id' topic comment time) =
    pairs ("id" .= id' <> "topic" .= topic <> "comment" .= comment <> "time" .= time)
