{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}

module ScrapBook.Fetch.Internal
  ( Fetch (..)
  , fetchHtml
  , throwFetchError
  ) where

import           RIO

import           Data.Default          (def)
import           Data.Extensible
import           Data.Proxy            (Proxy (..))
import           Data.Text.Conversions (UTF8 (..), decodeConvertText)
import           Network.HTTP.Req
import           ScrapBook.Collecter
import           ScrapBook.Data.Site   (Post, Site)

class Fetch kv where
  fetchFrom :: proxy kv -> Site -> AssocValue kv -> Collecter [Post]

fetchHtml :: Text -> Collecter Text
fetchHtml url = do
  result <- get' url bsResponse
  case result of
    Left err   -> throwFetchError (Left err)
    Right resp ->
      pure . fromMaybe "" $ decodeConvertText (UTF8 $ responseBody resp)

get' :: HttpResponse r => Text -> Proxy r -> Collecter (Either HttpException r)
get' url proxy =
  case parseUrlHttp (encodeUtf8 url) of
    Just (url', opts) ->
      runReq' def (req GET url' NoReqBody proxy opts) <* sleep' 1000
    Nothing ->
      case parseUrlHttps (encodeUtf8 url) of
        Just (url', opts) ->
          runReq' def (req GET url' NoReqBody proxy opts) <* sleep' 1000
        Nothing ->
          throwFetchError (Right $ "cannot parse url: " <> url)
  where
    sleep' :: Int -> Collecter ()
    sleep' n = logInfo (display $ "fethed: " <> url) *> threadDelay n


runReq' :: (MonadIO m) => HttpConfig -> Req a -> m (Either HttpException a)
runReq' conf = liftIO . try . runReq conf

throwFetchError :: Either HttpException Text -> Collecter a
throwFetchError = throwM . FetchException
