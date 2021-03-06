{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ScrapBook.Feed
  ( writeFeed
  ) where

import           RIO
import           RIO.FilePath
import qualified RIO.Text                 as T
import qualified RIO.Text.Lazy            as T.Lazy

import           Data.Extensible
import           ScrapBook.Collecter
import           ScrapBook.Data.Config
import           ScrapBook.Data.Site
import           ScrapBook.Feed.Atom
import           ScrapBook.Feed.RSS       (fromRSSFeed)
import           ScrapBook.Fetch.Internal (Fetch (..), fetchHtml,
                                           throwFetchError)
import           ScrapBook.Write.Internal (Write (..), throwWriteError)
import           Text.Feed.Import         (parseFeedString)
import           Text.Feed.Types          (Feed (..))
import qualified Text.XML                 as XML

instance Fetch ("feed" >: Text) where
  fetchFrom _ site url = do
    resp <- T.unpack <$> fetchHtml url
    case parseFeedString resp of
      Just (AtomFeed feed) -> pure $ fromAtomFeed site (toAtomConfig url) feed
      Just (RSSFeed feed)  -> pure $ fromRSSFeed  site feed
      _                    -> throwFetchError (Right "can't parse feed.")

instance Write ("feed" >: ()) where
  writeTo _ conf posts = do
    conf' <-
      maybe (throwWriteError "add feed config on yaml.") pure $ conf ^. #feed
    case toDocument (toAtomFeed conf' posts) of
      Left err   -> throwWriteError $ mconcat (toList err)
      Right docs -> pure $ T.Lazy.toStrict (XML.renderText XML.def docs)
  fileName' _ conf = feedName $ fromMaybe mempty (conf ^. #feed)
  updateFileName' _ path conf =
    conf & #feed `over` fmap (over #name $ maybe (pure $ T.pack name) pure)
    where
      name = replaceExtension (takeFileName path) "xml"

writeFeed :: IsSiteFields xs =>
  FilePath -> FeedConfig -> [Post (Record xs)] -> Collecter ()
writeFeed dir conf posts =
  case toDocument (toAtomFeed conf posts) of
    Left err   -> throwWriteError $ mconcat (toList err)
    Right docs -> liftIO $ XML.writeFile XML.def path docs
  where
    path = mconcat [dir, "/", feedName conf]
