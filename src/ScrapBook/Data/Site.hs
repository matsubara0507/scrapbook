{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module ScrapBook.Data.Site
  ( Site
  , SiteId
  , AtomConfig
  , toAtomConfig
  , Post
  , Date
  , Url
  , toAbsoluteUrl
  , Summary (..)
  , summaryToText
  ) where

import           Control.Lens                      ((&), (.~), (^.))
import           Data.Default                      (def)
import           Data.Extensible
import           Data.Extensible.Instances.Default ()
import           Data.Map                          (Map)
import           Data.Text                         (Text, uncons)
import           ScrapBook.Internal.Utils          (toHost)
import           Text.Atom.Feed                    (Date)

type Site = Record
  '[ "title"  >: Text
   , "author" >: Text
   , "url"    >: Url
   , "id"     >: SiteId
   ]

type SiteId = Variant
  '[ "feed" >: Text
   , "atom" >: AtomConfig
   , "rss"  >: Text -- ^ RSS 2.0
   , "url"  >: Url
   ]

type AtomConfig = Record
  '[ "url"       >: Text
   , "linkAttrs" >: Maybe (Map Text Text)
   ]

toAtomConfig :: Url -> AtomConfig
toAtomConfig url = def & #url .~ url

type Post = Record
  '[ "title"   >: Text
   , "url"     >: Url
   , "date"    >: Date
   , "summary" >: Maybe Summary
   , "site"    >: Site
   ]

type Url = Text

data Summary
  = TextSummary Text
  | HtmlSummary Text
  deriving (Show, Eq)

summaryToText :: Summary -> Text
summaryToText (TextSummary txt) = txt
summaryToText (HtmlSummary txt) = txt

toAbsoluteUrl :: Site -> Url -> Url
toAbsoluteUrl site url =
  case uncons url of
    Just ('/', _) -> toHost (site ^. #url) `mappend` url
    _             -> url
