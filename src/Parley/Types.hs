{-# LANGUAGE OverloadedStrings #-}

module Parley.Types where

import qualified Data.ByteString.Lazy as LBS
import           Data.Text            (Text)
import           Data.Text.Encoding   (decodeUtf8)

data ParleyRequest = AddRequest Add
                   | ViewRequest Text

data Add = Add { topic   :: Text
               , comment :: Text
               }

data Error = NoTopicInRequest
           | UnknownRoute
           | NoCommentText

mkAddRequest :: Text -> LBS.ByteString -> Either Error ParleyRequest
mkAddRequest "" _ = Left NoTopicInRequest
mkAddRequest _ "" = Left NoCommentText
mkAddRequest t b =
  let body = decodeUtf8 . LBS.toStrict $ b
   in pure . AddRequest $ Add t body
