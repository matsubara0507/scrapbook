{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ScrapBook.Json
    (
    ) where

import           RIO
import           RIO.FilePath
import qualified RIO.HashMap              as M.Hash
import qualified RIO.Text                 as T

import           Data.Aeson               (ToJSON (..))
import qualified Data.Aeson               as JSON
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Extensible
import           Data.Functor.Identity
import           Data.Proxy               (Proxy (..))
import           GHC.TypeLits             (KnownSymbol, symbolVal)
import           ScrapBook.Data.Site      (Summary (..))
import           ScrapBook.Write.Internal (Write (..), throwWriteError)

instance Write ("json" >: ()) where
  writeTo _ _conf =
    either (throwWriteError . tshow) pure . decodeUtf8' . toStrictBytes . encodePretty
  fileName' _ conf = maybe "posts.json" T.unpack $ conf ^. #json
  updateFileName' _ path conf =
    conf & #json `over` maybe (pure $ T.pack name) pure
    where
      name = replaceExtension (takeFileName path) "json"

instance Forall (KeyValue KnownSymbol ToJSON) xs => ToJSON (Variant xs) where
  toJSON = matchField $
    htabulateFor (Proxy :: Proxy (KeyValue KnownSymbol ToJSON)) $ \m ->
      let key = T.pack . symbolVal $ proxyAssocKey m
      in Field (Match $ JSON.Object . kvToJSON key . runIdentity)

kvToJSON :: ToJSON v => Text -> v -> JSON.Object
kvToJSON key val =
  M.Hash.fromList [("type", JSON.String key), ("value", toJSON val)]

instance ToJSON Summary where
  toJSON (TextSummary txt) = JSON.Object $ kvToJSON "text" txt
  toJSON (HtmlSummary txt) = JSON.Object $ kvToJSON "html" txt
