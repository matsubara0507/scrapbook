{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ScrapBook.Feed.Atom
  ( fromAtomFeed
  , toAtomFeed
  , toEntry
  , fromEntry
  , toDocument
  ) where

import           RIO

import           Control.Lens                      ((<&>))
import           Data.Extensible
import           Data.Extensible.Instances.Default ()
import           Data.List                         (sortOn)
import qualified Data.Map                          as Map
import           Data.Maybe                        (listToMaybe)
import           Data.Set                          (Set)
import           Data.String                       (fromString)
import           Data.Text                         (Text, pack, unpack)
import qualified Data.XML.Types                    as XML (Content (..))
import           ScrapBook.Data.Config
import           ScrapBook.Data.Site
import qualified ScrapBook.Feed.Atom.Internal      as My
import           ScrapBook.Fetch.Internal          (Fetch (..), fetchHtml,
                                                    throwFetchError)
import qualified Text.Atom.Feed                    as Atom
import           Text.Feed.Import                  (parseFeedString)
import           Text.Feed.Types                   (Feed (..))
import qualified Text.XML                          as XML

instance Fetch ("atom" >: AtomConfig) where
  fetchFrom _ site conf = do
    resp <- unpack <$> fetchHtml (conf ^. #url)
    case parseFeedString resp of
      Just (AtomFeed feed) -> pure $ fromAtomFeed site conf feed
      _                    -> throwFetchError (Right "can't parse atom feed.")

fromAtomFeed :: Site -> AtomConfig -> Atom.Feed -> [Post]
fromAtomFeed site conf feed = fromEntry site conf <$> Atom.feedEntries feed

toAtomFeed :: FeedConfig -> [Post] -> Atom.Feed
toAtomFeed conf posts =
  (Atom.nullFeed
    (mconcat [conf ^. #baseUrl, "/", pack $ feedName conf])
    (Atom.TextString $ conf ^. #title)
    (maybe "" (view #date) $ listToMaybe posts'))
    { Atom.feedEntries = map toEntry posts'
    , Atom.feedLinks = [Atom.nullLink $ conf ^. #baseUrl]
    }
  where
    posts' = reverse $ sortOn (view #date) posts

toEntry :: Post -> Atom.Entry
toEntry post =
  (Atom.nullEntry
    (post ^. #url) (Atom.TextString $ post ^. #title) (post ^. #date))
    { Atom.entryAuthors =
        [ Atom.nullPerson { Atom.personName = post ^. #site ^. #author } ]
    , Atom.entryLinks = [Atom.nullLink $ post ^. #url]
    , Atom.entrySummary = fmap fromSummary (post ^. #summary)
    }

fromEntry :: Site -> AtomConfig -> Atom.Entry -> Post
fromEntry site conf entry
    = #title   @= txtToText (Atom.entryTitle entry)
   <: #url     @= toUrl site conf entry
   <: #date    @= Atom.entryUpdated entry
   <: #summary @= (toSummary =<< Atom.entrySummary entry)
   <: #site    @= site
   <: nil

txtToText :: Atom.TextContent -> Text
txtToText = pack . Atom.txtToString

toDocument :: Atom.Feed -> Either (Set Text) XML.Document
toDocument feed = XML.fromXMLElement (My.xmlFeed feed)
  <&> \elm -> XML.Document (XML.Prologue [] Nothing []) elm []

toUrl :: Site -> AtomConfig -> Atom.Entry -> Text
toUrl site conf entry =
  maybe ""
    (toAbsoluteUrl site . Atom.linkHref )
    (listToMaybe . filter (p . Atom.linkAttrs) $ Atom.entryLinks entry)
  where
    p attrs = all (`elem` attrs) $
      toAttr <$> (maybe [] Map.toList $ conf ^. #linkAttrs)

toAttr :: (Text, Text) -> Atom.Attr
toAttr (k, v) = (fromString $ unpack k, [XML.ContentText v])

toSummary :: Atom.TextContent -> Maybe Summary
toSummary (Atom.TextString txt) = Just $ TextSummary txt
toSummary (Atom.HTMLString txt) = Just $ HtmlSummary txt
toSummary _                     = Nothing

fromSummary :: Summary -> Atom.TextContent
fromSummary (TextSummary txt) = Atom.TextString txt
fromSummary (HtmlSummary txt) = Atom.HTMLString txt
