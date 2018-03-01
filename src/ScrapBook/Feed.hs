{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ScrapBook.Feed
  ( writeFeed
  , toFeed
  , toEntry
  , fromEntry
  ) where

import           Control.Lens              (view, (^.))
import           Control.Monad.Error.Class (throwError)
import           Control.Monad.IO.Class    (liftIO)
import           Data.Extensible
import           Data.List                 (sortOn)
import           Data.Maybe                (fromMaybe, listToMaybe)
import           Data.Set                  (toList)
import           Data.Text                 (Text, pack, unpack)
import           ScrapBook.Collecter
import           ScrapBook.Data.Config
import           ScrapBook.Data.Site
import           ScrapBook.Fetch.Internal  (Fetch (..), fetchHtml,
                                            throwFetchError)
import qualified Text.Atom.Feed            as Atom
import qualified Text.Atom.Feed.Export     as Export
import           Text.Feed.Import          (parseFeedString)
import           Text.Feed.Types           (Feed (..))
import qualified Text.XML                  as XML

instance Fetch ("feed" >: Text) where
  fetchFrom _ site feedUrl = do
    resp <- unpack <$> fetchHtml feedUrl
    case parseFeedString resp of
      Just (AtomFeed feed) -> pure $ fromEntry site <$> Atom.feedEntries feed
      _                    -> throwFetchError (Right "can't parse atom feed.")

writeFeed :: FeedConfig -> [Post] -> Collecter ()
writeFeed conf posts = do
  case XML.fromXMLElement $ Export.xmlFeed (toFeed conf posts) of
    Left err -> throwError . CollectException $ mconcat (toList err)
    Right element -> liftIO $ XML.writeFile XML.def
        (unpack . fromMaybe "atom.xml" $ conf ^. #name)
        (XML.Document (XML.Prologue [] Nothing []) element [])

toFeed :: FeedConfig -> [Post] -> Atom.Feed
toFeed conf posts =
  (Atom.nullFeed
    (mconcat [conf ^. #baseUrl, "/", fromMaybe "atom.xml" (conf ^. #name)])
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
    }

fromEntry :: Site -> Atom.Entry -> Post
fromEntry site entry
    = #title @= txtToText (Atom.entryTitle entry)
   <: #url   @= Atom.entryId entry
   <: #date  @= Atom.entryUpdated entry
   <: #site  @= site
   <: nil

txtToText :: Atom.TextContent -> Text
txtToText = pack . Atom.txtToString
