{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ScrapBook.Feed
  ( toEntry
  , fromEntry
  ) where

import           Control.Lens             ((^.))
import           Data.Extensible
import           Data.Text                (Text, pack, unpack)
import           ScrapBook.Data.Site
import           ScrapBook.Fetch.Internal (Fetch (..), fetchHtml,
                                           throwFetchError)
import           Text.Atom.Feed
import           Text.Feed.Import         (parseFeedString)
import           Text.Feed.Types          (Feed (..))

instance Fetch ("feed" >: Text) where
  fetchFrom _ site feedUrl = do
    resp <- unpack <$> fetchHtml feedUrl
    case parseFeedString resp of
      Just (AtomFeed feed) -> pure $ fromEntry site <$> feedEntries feed
      _                    -> throwFetchError (Right "can't parse atom feed.")

toEntry :: Post -> Entry
toEntry post =
  (nullEntry (post ^. #url) (TextString $ post ^. #title) (post ^. #date))
    { entryAuthors = [ nullPerson { personName = post ^. #site ^. #author } ]
    }

fromEntry :: Site -> Entry -> Post
fromEntry site entry
    = #title @= txtToText (entryTitle entry)
   <: #url   @= entryId entry
   <: #date  @= entryUpdated entry
   <: #site  @= site
   <: nil

txtToText :: TextContent -> Text
txtToText = pack . txtToString
