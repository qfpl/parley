{-# LANGUAGE OverloadedStrings #-}

module Parley.Main where

import           Control.Monad            ((<=<))

import qualified Data.ByteString.Lazy     as LBS
import           Data.Monoid              ((<>))
import           Data.Text                (Text)
import           Data.Text.Encoding       (encodeUtf8)

import qualified Network.HTTP.Types       as HT
import           Network.Wai              (Request, Response, ResponseReceived,
                                           pathInfo, responseLBS,
                                           strictRequestBody)
import           Network.Wai.Handler.Warp (run)

import           Parley.Types             (Add, Error (..), ParleyRequest (..),
                                           mkAddRequest, topic)

main :: IO ()
main =
  run 8080 app

app :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app request response = do
  let pathBits = pathInfo request
  body <- strictRequestBody request
  let rsp = handleRequest <=< mkRequest pathBits $ body
  response $ handleResponse rsp

mkRequest :: [Text] -> LBS.ByteString -> Either Error ParleyRequest
mkRequest path body =
  case path of
    ("add":t:[])  -> mkAddRequest t body
    ("view":t:[]) -> pure $ ViewRequest t
    _             -> Left UnknownRoute

handleRequest :: ParleyRequest -> Either Error Response
handleRequest r =
  case r of
    AddRequest ar -> handleAdd ar
    ViewRequest t -> handleView t

handleResponse :: Either Error Response -> Response
handleResponse (Right r) = r
handleResponse (Left e) =
  case e of
    NoTopicInRequest -> rsp HT.status404 "Topic was expected as the next URI component, but it was empty"
    UnknownRoute     -> rsp HT.status404 $ "Whatever you're looking for - it isn't here"
    NoCommentText    -> rsp HT.status400 $ "Bad request: expected body text"
  where rsp s t = responseLBS s [("Content-Type", "text/plain")] t

handleAdd :: Add -> Either Error Response
handleAdd ar =
  pure $ responseLBS HT.status200
                     contentPlainText
                     (tToBS $ "I should be adding to '" <> topic ar <> "'!")

handleView :: Text -> Either Error Response
handleView _t = error "TODO: handle view with no topic"

viewTopic :: Text -> Either Error Response
viewTopic = error "TODO: viewTopic"

contentPlainText :: HT.ResponseHeaders
contentPlainText = [("Content-Type", "text/plain")]

tToBS :: Text -> LBS.ByteString
tToBS = LBS.fromStrict . encodeUtf8
