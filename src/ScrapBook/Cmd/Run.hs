{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module ScrapBook.Cmd.Run
    ( run
    , run'
    ) where

import           RIO

import           Data.Yaml (ParseException)
import           ScrapBook

run :: MonadUnliftIO m => Format -> Config -> m Text
run fmt conf = do
  results <- forM (conf ^. #sites) $ \site ->
    collect (fetch $ toSite site) `catch` handler
  collect $ write conf fmt (concat results)
  where
    handler :: MonadUnliftIO m => CollectError -> m [Post]
    handler e = collect (logError $ displayShow e) >> pure []

run' :: (MonadUnliftIO m, MonadThrow m) =>
  Format -> Either ParseException Config -> m (Config, Text)
run' fmt = \case
  Left err   -> throwM $ YamlParseException err
  Right conf -> (,) conf <$> run fmt conf
