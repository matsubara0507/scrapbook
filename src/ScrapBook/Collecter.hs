{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module ScrapBook.Collecter where

import           RIO
import qualified RIO.Text         as T

import           Data.Yaml        (ParseException)
import           Network.HTTP.Req (HttpException)

type Collecter = RIO Env

newtype Env = Env { logFunc :: LogFunc }

instance HasLogFunc Env where
  logFuncL = lens logFunc (\x y -> x { logFunc = y })

data CollectError
  = FetchException (Either HttpException Text)
  | WriteException Text
  | CollectException Text
  | YamlParseException ParseException
  deriving (Typeable)

instance Exception CollectError

instance Show CollectError where
  show = \case
    FetchException (Left e)  -> "fetch phase error: " <> show e
    FetchException (Right t) -> "fetch phase error: " <> T.unpack t
    WriteException t         -> "write phase error: " <> T.unpack t
    CollectException t       -> "error (no phase): " <> T.unpack t
    YamlParseException e     -> "reading yaml error: " <> show e

collect :: MonadUnliftIO m => Collecter a -> m a
collect f = do
  opt <- logOptionsHandle stdout False
  withLogFunc opt $ \logFunc -> runRIO Env{..} f
