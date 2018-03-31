{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module ScrapBook.Cmd.Run
    ( run
    , run'
    ) where

import           RIO

import           Data.Text (Text)
import           Data.Yaml (ParseException)
import           ScrapBook

run :: Format -> Config -> IO (Either CollectError Text)
run fmt conf = collect $
  (write conf fmt . concat) =<< mapM (fetch . toSite) (conf ^. #sites)

run' :: Format -> Either ParseException Config -> IO (Either CollectError (Config, Text))
run' fmt = \case
  Left err -> pure $ Left (YamlParseException err)
  Right conf -> fmap ((,) conf) <$> run fmt conf
