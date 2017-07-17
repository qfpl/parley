{-# LANGUAGE OverloadedStrings #-}

module Parley.Main where

import           Control.Exception.Base     (bracket)

import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import           Data.Text.Encoding         (encodeUtf8)
import           Database.SQLite.Simple     (Connection)

import qualified Network.HTTP.Types         as HT
import           Network.Wai                (Request, Response,
                                             ResponseReceived, pathInfo,
                                             responseLBS, strictRequestBody)
import           Network.Wai.Handler.Warp   (run)

import           Parley.DB                  (closeDB, getCommentsForTopic,
                                             initDB)
import           Parley.Types               (Add, Error (..),
                                             ParleyRequest (..), addTopic,
                                             mkAddRequest)

main :: IO ()
main =
  bracket (initDB "test.sqlite" "comments")
          closeDB
          (run 8080 . app)

app :: Connection -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app conn request response = do
  rq <- mkRequest request
  rsp <- either (pure . Left) (handleRequest conn) rq
  response $ either handleError id rsp

mkRequest :: Request -> IO (Either Error ParleyRequest)
mkRequest request =
  case pathInfo request of
    [t,"add"]  -> mkAddRequest t <$> strictRequestBody request
    [t,"view"] -> pure . pure $ ViewRequest t
    _          -> pure $ Left UnknownRoute

handleRequest :: Connection -> ParleyRequest -> IO (Either Error Response)
handleRequest conn r = do
  case r of
    AddRequest ar -> handleAdd conn ar
    ViewRequest t -> handleView conn t

handleError :: Error -> Response
handleError e =
  case e of
    NoTopicInRequest -> rsp HT.status404 "Topic was expected as the next URI component, but it was empty"
    UnknownRoute     -> rsp HT.status404 $ "Whatever you're looking for - it isn't here"
    NoCommentText    -> rsp HT.status400 $ "Bad request: expected body text"
  where rsp s t = responseLBS s [("Content-Type", "text/plain")] t

handleAdd :: Connection -> Add -> IO (Either Error Response)
handleAdd _conn ar =
  pure . pure $ responseLBS HT.status200
                     contentPlainText
                     (tToBS $ "I should be adding to '" <> addTopic ar <> "'!")

handleView :: Connection -> Text -> IO (Either Error Response)
handleView conn topic = do
  comments <- getCommentsForTopic conn topic
  let rsp = responseLBS HT.status200 contentPlainText $ "Have " <> L8.pack (show (length comments)) <> " comments for topic '" <> tToBS topic <> "'"
  pure $ Right rsp

contentPlainText :: HT.ResponseHeaders
contentPlainText = [("Content-Type", "text/plain")]

tToBS :: Text -> LBS.ByteString
tToBS = LBS.fromStrict . encodeUtf8
