{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module ScrapBook.Data.Site
  ( Site
  , SiteFields
  , IsSiteFields
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

import           RIO
import qualified RIO.Text                     as T

import           Data.Aeson                   (ToJSON (..))
import qualified Data.Aeson                   as JSON
import           Data.Extensible
import           ScrapBook.Internal.Instances (kvToJSON)
import           ScrapBook.Internal.Utils     (toHost)
import           Text.Atom.Feed               (Date)

type Site = Record SiteFields

type SiteFields =
  '[ "title"  >: Text
   , "author" >: Text
   , "url"    >: Url
   , "id"     >: SiteId
   ]

type IsSiteFields xs =
  ( Forall (KeyTargetAre KnownSymbol (Instance1 ToJSON Identity)) xs
  , Lookup xs "title" Text
  , Lookup xs "author" Text
  , Lookup xs "url" Url
  , Lookup xs "id" SiteId
  )

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
toAtomConfig url = mempty & #url `set` url

type Post s = Record
  '[ "title"   >: Text
   , "url"     >: Url
   , "date"    >: Date
   , "summary" >: Maybe Summary
   , "site"    >: s
   ]

type Url = Text

data Summary
  = TextSummary Text
  | HtmlSummary Text
  deriving (Show, Eq)

instance ToJSON Summary where
  toJSON (TextSummary txt) = JSON.Object $ kvToJSON "text" txt
  toJSON (HtmlSummary txt) = JSON.Object $ kvToJSON "html" txt

summaryToText :: Summary -> Text
summaryToText (TextSummary txt) = txt
summaryToText (HtmlSummary txt) = txt

-- |
-- if url have prefix `/`, append base url
toAbsoluteUrl :: IsSiteFields xs => Record xs -> Url -> Url
toAbsoluteUrl site url =
  case T.uncons url of
    Just ('/', _) -> toHost (site ^. #url) `mappend` url
    _             -> url
