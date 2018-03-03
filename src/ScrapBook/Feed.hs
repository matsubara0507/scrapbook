{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ScrapBook.Feed
  ( writeFeed
  ) where

import           Control.Lens                      ((^.))
import           Control.Monad.IO.Class            (liftIO)
import           Data.Default                      (def)
import           Data.Extensible
import           Data.Extensible.Instances.Default ()
import           Data.Maybe                        (fromMaybe)
import           Data.Set                          (toList)
import           Data.Text                         (Text)
import           Data.Text.Lazy                    (toStrict)
import           ScrapBook.Collecter
import           ScrapBook.Data.Config
import           ScrapBook.Data.Site
import           ScrapBook.Feed.Atom
import           ScrapBook.Fetch.Internal          (Fetch (..))
import           ScrapBook.Write.Internal          (Write (..), throwWriteError)
import qualified Text.XML                          as XML

instance Fetch ("feed" >: Text) where
  fetchFrom _ = fetchFromAtomFeed

instance Write ("feed" >: ()) where
  writeTo _ conf posts = do
    conf' <-
      maybe (throwWriteError "add feed config on yaml.") pure $ conf ^. #feed
    case toDocument (toFeed conf' posts) of
      Left err   -> throwWriteError $ mconcat (toList err)
      Right docs -> pure $ toStrict (XML.renderText def docs)
  fileName' _ conf = feedName $ fromMaybe def (conf ^. #feed)

writeFeed :: FilePath -> FeedConfig -> [Post] -> Collecter ()
writeFeed dir conf posts =
  case toDocument (toFeed conf posts) of
    Left err   -> throwWriteError $ mconcat (toList err)
    Right docs -> liftIO $ XML.writeFile def path docs
  where
    path = mconcat [dir, "/", feedName conf]
