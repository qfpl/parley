{-# LANGUAGE OverloadedStrings #-}

module Parley.Main where

import           Control.Exception.Base     (bracket)

import           Data.Aeson                 (ToJSON, encode)
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import           Data.Text.Encoding         (encodeUtf8)

import qualified Network.HTTP.Types         as HT
import           Network.Wai                (Request, Response,
                                             ResponseReceived, pathInfo,
                                             responseLBS, strictRequestBody)
import           Network.Wai.Handler.Warp   (run)

import           Parley.Config              (Config (..), Port (..),
                                             parseOptions)
import           Parley.DB                  (ParleyDb, addCommentToTopic,
                                             closeDB, getComments, getTopics,
                                             initDB)
import           Parley.Types               (CommentText, ContentType (..),
                                             Error (..), ParleyRequest (..),
                                             Topic (getTopic), mkAddRequest,
                                             mkViewRequest, render)

main :: IO ()
main = do
  eConfig <- parseOptions "parley.json"
  case eConfig of
    Left e -> putStrLn ("Error parsing config: " <> show e)
    Right config -> runWithConfig config

runWithConfig :: Config
              -> IO ()
runWithConfig c = do
  let port' = fromIntegral . unPort $ port c
      runWithConn conn = bracket (pure conn) closeDB (run port' . app)
  eConn <- initDB (dbPath c) "comments"
  case eConn of
    Left e -> putStrLn ("Error initialisting DB: " <> show e)
    Right conn -> runWithConn conn

app :: ParleyDb
    -> Request
    -> (Response -> IO ResponseReceived)
    -> IO ResponseReceived
app db request cb = do
  let handleRq (Left e)  = pure (Left e)
      handleRq (Right r) = handleRequest db r

      handleRsp (Left e)    = handleError e
      handleRsp (Right rsp) = rsp

  erq <- mkRequest request
  ersp <- handleRq erq
  cb (handleRsp ersp)

mkRequest :: Request
          -> IO (Either Error ParleyRequest)
mkRequest request =
  case pathInfo request of
    [t,"add"]  -> mkAddRequest t <$> strictRequestBody request
    [t,"view"] -> pure $ mkViewRequest t
    ["list"]   -> pure (Right ListRequest)
    _          -> pure (Left UnknownRoute)

handleRequest :: ParleyDb
              -> ParleyRequest
              -> IO (Either Error Response)
handleRequest conn rq =
  case rq of
    AddRequest t c -> handleAdd conn t c
    ViewRequest t  -> dbJSONResponse $ getComments conn t
    ListRequest    -> dbJSONResponse $ getTopics conn

handleError :: Error -> Response
handleError e =
  case e of
    NoTopicInRequest ->
      rsp HT.status400 "Empty topics not allowed"
    UnknownRoute ->
      rsp HT.status404 "Not found :("
    NoCommentText ->
      rsp HT.status400 "Empty body text not allowed"
    SQLiteError se ->
      rsp HT.status500 (dbError se)
  where
    rsp s t = responseLBS s [contentHeader PlainText] t
    dbError se = "Database error: " <> LBS8.pack (show se)

handleAdd :: ParleyDb
          -> Topic
          -> CommentText
          -> IO (Either Error Response)
handleAdd conn t c =
  let addResult = addCommentToTopic conn t c
      success = const (successfulAddResponse t)
   in fmap (fmap success) addResult

successfulAddResponse :: Topic -> Response
successfulAddResponse t =
  responseLBS HT.status200
              [contentHeader PlainText]
              (tToBS ("Successfully added a comment to '"
                      <> getTopic t <> "'"))

dbJSONResponse :: ToJSON a
               => IO (Either Error a)
               -> IO (Either Error Response)
dbJSONResponse iea = do
  let responseFromJSON a =
        responseLBS HT.status200
                    [contentHeader JSON]
                    (encode a)
  ea <- iea
  pure (fmap responseFromJSON ea)

contentHeader :: ContentType -> HT.Header
contentHeader ct = ("Content-Type", render ct)

tToBS :: Text -> LBS.ByteString
tToBS = LBS.fromStrict . encodeUtf8
