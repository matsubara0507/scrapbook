{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ScrapBook.Json
    (
    ) where

import           RIO
import           RIO.FilePath
import qualified RIO.Text                 as T

import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Extensible
import           ScrapBook.Write.Internal (Write (..), throwWriteError)

instance Write ("json" >: ()) where
  writeTo _ _conf =
    either (throwWriteError . tshow) pure . decodeUtf8' . toStrictBytes . encodePretty
  fileName' _ conf = maybe "posts.json" T.unpack $ conf ^. #json
  updateFileName' _ path conf =
    conf & #json `over` maybe (pure $ T.pack name) pure
    where
      name = replaceExtension (takeFileName path) "json"
