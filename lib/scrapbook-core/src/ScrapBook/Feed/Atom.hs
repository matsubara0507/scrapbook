{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ScrapBook.Feed.Atom
  ( fromAtomFeed
  , toAtomFeed
  , toEntry
  , fromEntry
  , toDocument
  ) where

import qualified Data.List                    as L
import           RIO
import qualified RIO.Map                      as M
import qualified RIO.Text                     as T

import           Data.Extensible
import qualified Data.Ord                     as Ord
import qualified Data.XML.Types               as XML (Content (..))
import           ScrapBook.Data.Config
import           ScrapBook.Data.Site
import qualified ScrapBook.Feed.Atom.Internal as My
import           ScrapBook.Fetch.Internal     (Fetch (..), fetchHtml,
                                               throwFetchError)
import qualified Text.Atom.Feed               as Atom
import           Text.Feed.Import             (parseFeedString)
import           Text.Feed.Types              (Feed (..))
import qualified Text.XML                     as XML

instance Fetch ("atom" >: AtomConfig) where
  fetchFrom _ site conf = do
    resp <- T.unpack <$> fetchHtml (conf ^. #url)
    case parseFeedString resp of
      Just (AtomFeed feed) -> pure $ fromAtomFeed site conf feed
      _                    -> throwFetchError (Right "can't parse atom feed.")

fromAtomFeed ::
  IsSiteFields xs => Record xs -> AtomConfig -> Atom.Feed -> [Post (Record xs)]
fromAtomFeed site conf feed = fromEntry site conf <$> Atom.feedEntries feed

toAtomFeed :: IsSiteFields xs => FeedConfig -> [Post (Record xs)] -> Atom.Feed
toAtomFeed conf posts =
  (Atom.nullFeed
    (mconcat [conf ^. #baseUrl, "/", T.pack $ feedName conf])
    (Atom.TextString $ conf ^. #title)
    (maybe "" (view #date) $ listToMaybe posts'))
    { Atom.feedEntries = map toEntry posts'
    , Atom.feedLinks = [Atom.nullLink $ conf ^. #baseUrl]
    }
  where
    posts' = L.sortOn (Ord.Down . view #date) posts

toEntry :: IsSiteFields xs => Post (Record xs) -> Atom.Entry
toEntry post =
  (Atom.nullEntry
    (post ^. #url) (Atom.TextString $ post ^. #title) (post ^. #date))
    { Atom.entryAuthors =
        [ Atom.nullPerson { Atom.personName = post ^. #site ^. #author } ]
    , Atom.entryLinks = [Atom.nullLink $ post ^. #url]
    , Atom.entrySummary = fmap fromSummary (post ^. #summary)
    }

fromEntry ::
  IsSiteFields xs => Record xs -> AtomConfig -> Atom.Entry -> Post (Record xs)
fromEntry site conf entry
    = #title   @= txtToText (Atom.entryTitle entry)
   <: #url     @= toUrl site conf entry
   <: #date    @= Atom.entryUpdated entry
   <: #summary @= (toSummary =<< Atom.entrySummary entry)
   <: #site    @= site
   <: nil

txtToText :: Atom.TextContent -> Text
txtToText = T.pack . Atom.txtToString

toDocument :: Atom.Feed -> Either (Set Text) XML.Document
toDocument feed = XML.fromXMLElement (My.xmlFeed feed)
  <&> \elm -> XML.Document (XML.Prologue [] Nothing []) elm []

toUrl :: IsSiteFields xs => Record xs -> AtomConfig -> Atom.Entry -> Text
toUrl site conf entry =
  maybe ""
    (toAbsoluteUrl site . Atom.linkHref )
    (listToMaybe . filter (p . Atom.linkAttrs) $ Atom.entryLinks entry)
  where
    p attrs = all (`elem` attrs) $
      toAttr <$> maybe [] M.toList (conf ^. #linkAttrs)

toAttr :: (Text, Text) -> Atom.Attr
toAttr (k, v) = (fromString $ T.unpack k, [XML.ContentText v])

toSummary :: Atom.TextContent -> Maybe Summary
toSummary (Atom.TextString txt) = Just $ TextSummary txt
toSummary (Atom.HTMLString txt) = Just $ HtmlSummary txt
toSummary _                     = Nothing

fromSummary :: Summary -> Atom.TextContent
fromSummary (TextSummary txt) = Atom.TextString txt
fromSummary (HtmlSummary txt) = Atom.HTMLString txt
