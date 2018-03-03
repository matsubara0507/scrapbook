{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ScrapBook.Feed.RSS
  ( fromRSSFeed
  , fromEntry
  ) where

import           Data.Extensible
import           Data.Extensible.Instances.Default ()
import           Data.Maybe                        (fromMaybe)
import           Data.Text                         (Text, unpack)
import           ScrapBook.Data.Site
import           ScrapBook.Fetch.Internal          (Fetch (..), fetchHtml,
                                                    throwFetchError)
import           ScrapBook.Internal.Utils          (formatTimeFromRFC822)
import           Text.Feed.Import                  (parseFeedString)
import           Text.Feed.Types                   (Feed (..))
import           Text.RSS.Syntax                   (RSS, RSSItem)
import qualified Text.RSS.Syntax                   as RSS

instance Fetch ("rss" >: Text) where
  fetchFrom _ site feedUrl = do
    resp <- unpack <$> fetchHtml feedUrl
    case parseFeedString resp of
      Just (RSSFeed feed) -> pure $ fromRSSFeed site feed
      _                   -> throwFetchError (Right "can't parse rss feed.")

fromRSSFeed :: Site -> RSS -> [Post]
fromRSSFeed site feed = fromEntry site <$> RSS.rssItems (RSS.rssChannel feed)

fromEntry :: Site -> RSSItem -> Post
fromEntry site entry
    = #title   @= fromMaybe "" (RSS.rssItemTitle entry)
   <: #url     @= toUrl site entry
   <: #date    @= fromMaybe "" (formatTimeFromRFC822 =<< RSS.rssItemPubDate entry)
   <: #summary @= HtmlSummary <$> RSS.rssItemDescription entry
   <: #site    @= site
   <: nil

toUrl :: Site -> RSSItem -> Text
toUrl site entry =
  maybe "" (toAbsoluteUrl site) (RSS.rssItemLink entry)
