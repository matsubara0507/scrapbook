{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module ScrapBook.Data.Site
  ( Site
  , SiteId
  , Post
  , Date
  ) where

import           Data.Extensible
import           Data.Text       (Text)
import           Text.Atom.Feed  (Date)

type Site = Record
  '[ "title"  >: Text
   , "author" >: Text
   , "url"    >: Text
   , "id"     >: SiteId
   ]

type SiteId = Variant
  '[ "feed" >: Text
   , "url"  >: Text
   ]

type Post = Record
  '[ "title" >: Text
   , "url"   >: Text
   , "date"  >: Date
   , "site"  >: Site
   ]
