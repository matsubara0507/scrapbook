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

import           Control.Lens             ((%~), (&), (^.))
import           Data.Aeson               (ToJSON (..))
import qualified Data.Aeson               as JSON
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Extensible
import           Data.Functor.Identity
import           Data.HashMap.Strict      (fromList)
import           Data.Proxy               (Proxy (..))
import           Data.Text                (Text, pack, unpack)
import           Data.Text.Conversions
import           GHC.TypeLits             (KnownSymbol, symbolVal)
import           ScrapBook.Data.Site      (Summary (..))
import           ScrapBook.Write.Internal (Write (..), throwWriteError)
import           System.FilePath          (replaceExtension, takeFileName)

instance Write ("json" >: ()) where
  writeTo _ _conf =
    maybe err pure . decodeConvertText . UTF8 . encodePretty
    where
      err = throwWriteError "can't decode Text from ByteString."
  fileName' _ conf = maybe "posts.json" unpack $ conf ^. #json
  updateFileName' _ path conf =
    conf & #json %~ maybe (pure $ pack name) pure
    where
      name = replaceExtension (takeFileName path) "json"

instance Forall (KeyValue KnownSymbol ToJSON) xs => ToJSON (Variant xs) where
  toJSON = matchField $
    htabulateFor (Proxy :: Proxy (KeyValue KnownSymbol ToJSON)) $ \m ->
      let key = pack . symbolVal $ proxyAssocKey m
      in Field (Match $ JSON.Object . kvToJSON key . runIdentity)

kvToJSON :: ToJSON v => Text -> v -> JSON.Object
kvToJSON key val = fromList [("type", JSON.String key), ("value", toJSON val)]

instance ToJSON Summary where
  toJSON (TextSummary txt) = JSON.Object $ kvToJSON "text" txt
  toJSON (HtmlSummary txt) = JSON.Object $ kvToJSON "html" txt
