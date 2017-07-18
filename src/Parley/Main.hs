{-# LANGUAGE OverloadedStrings #-}

module Parley.Main where

import           Control.Exception.Base             (bracket)

import           Data.Aeson                         (ToJSON, encode)
import qualified Data.ByteString.Char8              as BS8
import qualified Data.ByteString.Lazy               as LBS
import qualified Data.ByteString.Lazy.Char8         as LBS8
import           Data.Monoid                        ((<>))
import           Data.Text                          (Text)
import           Data.Text.Encoding                 (encodeUtf8)
import           Database.SQLite.Simple             (Connection)
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import qualified Network.HTTP.Types                 as HT
import           Network.Wai                        (Request, Response,
                                                     ResponseReceived, pathInfo,
                                                     responseLBS,
                                                     strictRequestBody)
import           Network.Wai.Handler.Warp           (run)

import           Parley.DB                          (addCommentToTopic, closeDB,
                                                     getComments, initDB, getTopics)
import           Parley.Types                       (Add (..), ContentType (..),
                                                     Error (..),
                                                     ParleyRequest (..),
                                                     mkAddRequest)

main :: IO ()
main = do
  eConn <- initDB "test.sqlite" "comments"
  either (putStrLn . ("Error initialisting DB: " <>) . show) runWithConn eConn
  where runWithConn conn =
          bracket (pure conn) closeDB (run 8080 . app)

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
    ["list"]   -> pure . pure $ ListRequest
    _          -> pure $ Left UnknownRoute

handleRequest :: Connection -> ParleyRequest -> IO (Either Error Response)
handleRequest conn r = do
  case r of
    AddRequest ar -> handleAdd conn ar
    ViewRequest t -> handleView conn t
    ListRequest   -> handleList conn

handleError :: Error -> Response
handleError e =
  case e of
    NoTopicInRequest     -> rsp HT.status404 "Topic was expected as the next URI component, but it was empty"
    UnknownRoute         -> rsp HT.status404 "Whatever you're looking for - it isn't here"
    NoCommentText        -> rsp HT.status400 "Bad request: expected body text"
    SQLiteError sqlError -> rsp HT.status500 $ "Database error: " <> LBS8.pack (show sqlError)
  where rsp s t = responseLBS s [contentHeader PlainText] t

handleAdd :: Connection -> Add -> IO (Either Error Response)
handleAdd conn (Add t c) = do
  addResult <- addCommentToTopic conn t c
  pure $ either (Left . SQLiteError) (Right . const (successfulAddResponse t)) addResult

successfulAddResponse :: Text -> Response
successfulAddResponse topic =
  responseLBS HT.status200
              [contentHeader PlainText]
              (tToBS $ "Successfully added a comment to '" <> topic <> "'")

handleView :: Connection -> Text -> IO (Either Error Response)
handleView conn topic = dbJSONResponse $ getComments conn topic

handleList :: Connection -> IO (Either Error Response)
handleList conn = dbJSONResponse $ getTopics conn

dbJSONResponse :: ToJSON a => IO (Either SQLiteResponse a) -> IO (Either Error Response)
dbJSONResponse =
  (=<<) (pure . either (Left . SQLiteError) (Right . responseFromJSON))

responseFromJSON :: ToJSON a => a -> Response
responseFromJSON =
  responseLBS HT.status200 [contentHeader JSON] . encode

contentHeader :: ContentType -> HT.Header
contentHeader ct = ("Content-Type", BS8.pack (show ct))

tToBS :: Text -> LBS.ByteString
tToBS = LBS.fromStrict . encodeUtf8
