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

import           Control.Lens                      (view, (<&>), (^.))
import           Control.Monad.IO.Class            (liftIO)
import           Data.Default                      (def)
import           Data.Extensible
import           Data.Extensible.Instances.Default ()
import           Data.List                         (sortOn)
import           Data.Maybe                        (fromMaybe, listToMaybe)
import           Data.Set                          (Set, toList)
import           Data.Text                         (Text, pack, unpack)
import           Data.Text.Lazy                    (toStrict)
import           ScrapBook.Collecter
import           ScrapBook.Data.Config
import           ScrapBook.Data.Site
import           ScrapBook.Fetch.Internal          (Fetch (..), fetchHtml,
                                                    throwFetchError)
import           ScrapBook.Write.Internal          (Write (..), throwWriteError)
import qualified Text.Atom.Feed                    as Atom
import qualified Text.Atom.Feed.Export             as Export
import           Text.Feed.Import                  (parseFeedString)
import           Text.Feed.Types                   (Feed (..))
import qualified Text.XML                          as XML

instance Fetch ("feed" >: Text) where
  fetchFrom _ site feedUrl = do
    resp <- unpack <$> fetchHtml feedUrl
    case parseFeedString resp of
      Just (AtomFeed feed) -> pure $ fromEntry site <$> Atom.feedEntries feed
      _                    -> throwFetchError (Right "can't parse atom feed.")

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

toFeed :: FeedConfig -> [Post] -> Atom.Feed
toFeed conf posts =
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
    , Atom.entryContent = Just (Atom.HTMLContent "")
    }

fromEntry :: Site -> Atom.Entry -> Post
fromEntry site entry
    = #title @= txtToText (Atom.entryTitle entry)
   <: #url   @= maybe "" Atom.linkHref (listToMaybe $ Atom.entryLinks entry)
   <: #date  @= Atom.entryUpdated entry
   <: #site  @= site
   <: nil

txtToText :: Atom.TextContent -> Text
txtToText = pack . Atom.txtToString

toDocument :: Atom.Feed -> Either (Set Text) XML.Document
toDocument feed = XML.fromXMLElement (Export.xmlFeed feed)
  <&> \elm -> XML.Document (XML.Prologue [] Nothing []) elm []
