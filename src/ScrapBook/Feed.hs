{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module ScrapBook.Feed where

import           Control.Lens             ((^.))
import           Data.Extensible
import           Data.Text                (Text, pack)
import           ScrapBook.Data.Site
import           ScrapBook.Fetch.Internal (Fetch (..))
import           Text.Atom.Feed

instance Fetch ("feed" >: Text) where
  fetchFrom _ _ _ = pure []

toEntry :: Post -> Entry
toEntry post =
  (nullEntry (post ^. #url) (TextString $ post ^. #title) (post ^. #date))
    { entryAuthors = [ nullPerson { personName = post ^. #site ^. #author } ]
    }

fromEntry ::  Site -> Entry -> Post
fromEntry site entry
    = #title @= txtToText (entryTitle entry)
   <: #url   @= entryId entry
   <: #date  @= entryUpdated entry
   <: #site  @= site
   <: nil

txtToText :: TextContent -> Text
txtToText = pack . txtToString
