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
import           ScrapBook.Data.Config (HasWriteConfigFields)
import           ScrapBook.Data.Site   (IsSiteFields, Post)

class Write kv where
  writeTo ::
    (IsSiteFields xs, HasWriteConfigFields ys) =>
    proxy kv -> Record ys -> [Post (Record xs)] -> Collecter Text
  fileName' ::
    HasWriteConfigFields ys =>
    proxy kv -> Record ys -> FilePath
  updateFileName' ::
    HasWriteConfigFields ys =>
    proxy kv -> FilePath -> Record ys -> Record ys

throwWriteError :: Text -> Collecter a
throwWriteError = throwM . WriteException
