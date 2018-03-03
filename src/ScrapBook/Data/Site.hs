{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module ScrapBook.Data.Site
  ( Site
  , SiteId
  , Post
  , Date
  , Url
  , toAbsoluteUrl
  ) where

import           Control.Lens             ((^.))
import           Data.Extensible
import           Data.Text                (Text, uncons)
import           ScrapBook.Internal.Utils (toHost)
import           Text.Atom.Feed           (Date)

type Site = Record
  '[ "title"  >: Text
   , "author" >: Text
   , "url"    >: Url
   , "id"     >: SiteId
   ]

type SiteId = Variant
  '[ "feed" >: Text
   , "url"  >: Url
   ]

type Post = Record
  '[ "title" >: Text
   , "url"   >: Url
   , "date"  >: Date
   , "site"  >: Site
   ]

type Url = Text

toAbsoluteUrl :: Site -> Url -> Url
toAbsoluteUrl site url =
  case uncons url of
    Just ('/', _) -> toHost (site ^. #url) `mappend` url
    _             -> url
