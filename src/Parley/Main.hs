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

import qualified Network.HTTP.Types                 as HT
import           Network.Wai                        (Request, Response,
                                                     ResponseReceived, pathInfo,
                                                     responseLBS,
                                                     strictRequestBody)
import           Network.Wai.Handler.Warp           (run)

import           Parley.Config                      (Config (..), Port (..), parseOptions)
import           Parley.DB                          (addCommentToTopic, closeDB,
                                                     getComments, getTopics,
                                                     initDB)
import           Parley.Types                       (CommentText,
                                                     ContentType (..),
                                                     Error (..),
                                                     ParleyRequest (..),
                                                     Topic (getTopic),
                                                     mkAddRequest,
                                                     mkViewRequest)

main :: IO ()
main =
  parseOptions "parley.json" >>=
    either (putStrLn . ("Error parsing config: " <>)) runWithConfig

runWithConfig :: Config
              -> IO ()
runWithConfig c = do
  let port' = fromIntegral . unPort $ port c
      runWithConn conn = bracket (pure conn) closeDB (run port' . app)
  eConn <- initDB (dbPath c) "comments"
  either (putStrLn . ("Error initialisting DB: " <>) . show) runWithConn eConn

app :: Connection
    -> Request
    -> (Response -> IO ResponseReceived)
    -> IO ResponseReceived
app conn request response = do
  rq <- mkRequest request
  rsp <- either (pure . Left) (handleRequest conn) rq
  response $ either handleError id rsp

mkRequest :: Request
          -> IO (Either Error ParleyRequest)
mkRequest request =
  case pathInfo request of
    [t,"add"]  -> mkAddRequest t <$> strictRequestBody request
    [t,"view"] -> pure $ mkViewRequest t
    ["list"]   -> pure (Right ListRequest)
    _          -> pure (Left UnknownRoute)

handleRequest :: Connection
              -> ParleyRequest
              -> IO (Either Error Response)
handleRequest conn r =
  case r of
    AddRequest topic comment -> handleAdd conn topic comment
    ViewRequest t            -> dbJSONResponse $ getComments conn t
    ListRequest              -> dbJSONResponse $ getTopics conn

handleError :: Error -> Response
handleError e =
  case e of
    NoTopicInRequest     -> rsp HT.status400 "Empty topics not allowed"
    UnknownRoute         -> rsp HT.status404 "Not found :("
    NoCommentText        -> rsp HT.status400 "Empty body text not allowed"
    SQLiteError sqlError -> rsp HT.status500 $ "Database error: " <> LBS8.pack (show sqlError)
  where rsp s t = responseLBS s [contentHeader PlainText] t

handleAdd :: Connection
          -> Topic
          -> CommentText
          -> IO (Either Error Response)
handleAdd conn t c = do
  addResult <- addCommentToTopic conn t c
  pure $ either (Left . SQLiteError) (Right . const (successfulAddResponse t)) addResult

successfulAddResponse :: Topic -> Response
successfulAddResponse t =
  responseLBS HT.status200
              [contentHeader PlainText]
              (tToBS $ "Successfully added a comment to '" <> getTopic t <> "'")

dbJSONResponse :: ToJSON a
               => IO (Either Error a)
               -> IO (Either Error Response)
dbJSONResponse =
  (=<<) (pure . fmap responseFromJSON)

responseFromJSON :: ToJSON a => a -> Response
responseFromJSON =
  responseLBS HT.status200 [contentHeader JSON] . encode

contentHeader :: ContentType -> HT.Header
contentHeader ct = ("Content-Type", BS8.pack (show ct))

tToBS :: Text -> LBS.ByteString
tToBS = LBS.fromStrict . encodeUtf8
