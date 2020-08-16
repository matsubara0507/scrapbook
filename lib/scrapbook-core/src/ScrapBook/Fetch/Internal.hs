{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}

module ScrapBook.Fetch.Internal
  ( Fetch (..)
  , fetchHtml
  , throwFetchError
  ) where

import           RIO

import           Data.Extensible
import           Network.HTTP.Req
import           ScrapBook.Collecter
import           ScrapBook.Data.Site (IsSiteFields, Post)
import qualified Text.URI            as URI

class Fetch kv where
  fetchFrom :: IsSiteFields xs =>
    proxy kv -> Record xs -> TargetOf kv -> Collecter [Post (Record xs)]

fetchHtml :: Text -> Collecter Text
fetchHtml url = do
  result <- get' url bsResponse
  case result of
    Left err   -> throwFetchError (Left err)
    Right resp ->
      either (throwFetchError . Right . tshow) pure $ decodeUtf8' (responseBody resp)

get' :: HttpResponse r => Text -> Proxy r -> Collecter (Either HttpException r)
get' url proxy = do
  uri <- URI.mkURI url
  case useURI uri of
    Just (Right (url', opts)) ->
      runReq' defaultHttpConfig (req GET url' NoReqBody proxy opts) <* sleep' 1000
    Just (Left (url', opts)) ->
      runReq' defaultHttpConfig (req GET url' NoReqBody proxy opts) <* sleep' 1000
    Nothing ->
      throwFetchError (Right $ "cannot parse url: " <> url)
  where
    sleep' :: Int -> Collecter ()
    sleep' n = logInfo (display $ "fethed: " <> url) *> threadDelay n


runReq' :: (MonadIO m) => HttpConfig -> Req a -> m (Either HttpException a)
runReq' conf = liftIO . try . runReq conf

throwFetchError :: Either HttpException Text -> Collecter a
throwFetchError = throwM . FetchException
