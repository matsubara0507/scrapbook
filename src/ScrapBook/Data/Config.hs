{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module ScrapBook.Data.Config where

import           Control.Lens                    ((^.))
import           Data.Extensible
import           Data.Extensible.Instances.Aeson ()
import           Data.Maybe                      (fromMaybe)
import           Data.Text                       (Text, unpack)
import           Data.Yaml
import           ScrapBook.Data.Site
import           ScrapBook.Internal.Utils        (embedM)

type Config = Record
  '[ "feed"  >: Maybe FeedConfig
   , "sites" >: [SiteConfig]
   ]

type FeedConfig = Record
  '[ "title"   >: Text
   , "baseUrl" >: Text
   , "name"    >: Maybe Text
   ]

type SiteConfig = Record
  '[ "title"  >: Text
   , "author" >: Text
   , "url"    >: Text
   , "feed"   >: Maybe Text
   ]

readConfig :: FilePath -> IO (Maybe Config)
readConfig = decodeFile

toSite :: SiteConfig -> Site
toSite conf
    = #title  @= (conf ^. #title)
   <: #author @= (conf ^. #author)
   <: #url    @= (conf ^. #url)
   <: #id     @= toSiteId conf
   <: nil

toSiteId :: SiteConfig -> SiteId
toSiteId conf = fromMaybe (embed $ #url @= conf ^. #url)
  $ embedM (#feed <@=> conf ^. #feed)

feedName :: FeedConfig -> FilePath
feedName conf = maybe "atom.xml" unpack (conf ^. #name)
