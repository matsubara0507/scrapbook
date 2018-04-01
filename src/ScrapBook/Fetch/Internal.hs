{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TemplateHaskell   #-}

module ScrapBook.Fetch.Internal
  ( Fetch (..)
  , fetchHtml
  , throwFetchError
  ) where

import           RIO                       hiding (logInfo)

import           Control.Monad.Error.Class (throwError)
import           Control.Monad.IO.Class    (MonadIO (..))
import           Control.Monad.Logger      (logInfo)
import           Data.Default              (def)
import           Data.Extensible
import           Data.Proxy                (Proxy (..))
import           Data.Text.Conversions     (UTF8 (..), decodeConvertText)
import           Network.HTTP.Req
import           ScrapBook.Collecter
import           ScrapBook.Data.Site       (Post, Site)
import           ScrapBook.Internal.Utils  (sleep)

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
      runReq' def (req GET url' NoReqBody proxy opts) <* sleep' 1
    Nothing ->
      case parseUrlHttps (encodeUtf8 url) of
        Just (url', opts) ->
          runReq' def (req GET url' NoReqBody proxy opts) <* sleep' 1
        Nothing ->
          throwFetchError (Right $ "cannot parse url: " <> url)
  where
    sleep' n = $(logInfo) ("fethed: " `mappend` url) *> sleep n


runReq' :: (MonadIO m) => HttpConfig -> Req a -> m (Either HttpException a)
runReq' conf = liftIO . try . runReq conf

throwFetchError :: Either HttpException Text -> Collecter a
throwFetchError = throwError . FetchException
