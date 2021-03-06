{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ScrapBook.Feed.RSS
  ( fromRSSFeed
  , fromEntry
  ) where

import           RIO
import qualified RIO.Text                 as T

import           Data.Extensible
import           ScrapBook.Data.Site
import           ScrapBook.Fetch.Internal (Fetch (..), fetchHtml,
                                           throwFetchError)
import           ScrapBook.Internal.Utils (formatTimeFromRFC822)
import           Text.Feed.Import         (parseFeedString)
import           Text.Feed.Types          (Feed (..))
import           Text.RSS.Syntax          (RSS, RSSItem)
import qualified Text.RSS.Syntax          as RSS

instance Fetch ("rss" >: Text) where
  fetchFrom _ site feedUrl = do
    resp <- T.unpack <$> fetchHtml feedUrl
    case parseFeedString resp of
      Just (RSSFeed feed) -> pure $ fromRSSFeed site feed
      _                   -> throwFetchError (Right "can't parse rss feed.")

fromRSSFeed :: IsSiteFields xs => Record xs -> RSS -> [Post (Record xs)]
fromRSSFeed site feed = fromEntry site <$> RSS.rssItems (RSS.rssChannel feed)

fromEntry :: IsSiteFields xs => Record xs -> RSSItem -> Post (Record xs)
fromEntry site entry
    = #title   @= fromMaybe "" (RSS.rssItemTitle entry)
   <: #url     @= toUrl site entry
   <: #date    @= fromMaybe "" (formatTimeFromRFC822 =<< RSS.rssItemPubDate entry)
   <: #summary @= HtmlSummary <$> RSS.rssItemDescription entry
   <: #site    @= site
   <: nil

toUrl :: IsSiteFields xs => Record xs -> RSSItem -> Text
toUrl site entry =
  maybe "" (toAbsoluteUrl site) (RSS.rssItemLink entry)
