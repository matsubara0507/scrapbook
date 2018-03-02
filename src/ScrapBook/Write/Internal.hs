{-# LANGUAGE PolyKinds #-}

module ScrapBook.Write.Internal
    ( Write (..)
    , throwWriteError
    ) where


import           Control.Monad.Error.Class (throwError)
import           Data.Text                 (Text)
import           ScrapBook.Collecter       (CollectError (..), Collecter)
import           ScrapBook.Data.Config     (Config)
import           ScrapBook.Data.Site       (Post)

class Write kv where
  writeTo :: proxy kv -> Config -> [Post] -> Collecter Text

throwWriteError ::  Text -> Collecter a
throwWriteError = throwError . WriteException
