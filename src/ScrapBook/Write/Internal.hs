{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE TypeFamilies     #-}

module ScrapBook.Write.Internal
    ( Write (..)
    , throwWriteError
    ) where

import           RIO

import           Data.Extensible
import           ScrapBook.Collecter   (CollectError (..), Collecter)
import           ScrapBook.Data.Config (Config)
import           ScrapBook.Data.Site   (IsSiteFields, Post)

class Write kv where
  writeTo :: IsSiteFields xs =>
    proxy kv -> Config -> [Post (Record xs)] -> Collecter Text
  fileName' :: proxy kv -> Config -> FilePath
  updateFileName' :: proxy kv -> FilePath -> Config -> Config

throwWriteError :: Text -> Collecter a
throwWriteError = throwM . WriteException
