{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module ScrapBook.Data.Config where

import           Control.Applicative             ((<|>))
import           Control.Lens                    ((^.))
import           Data.Extensible
import           Data.Extensible.Instances.Aeson ()
import           Data.Text                       (Text)
import           Data.Yaml
import           ScrapBook.Data.Site
import           ScrapBook.Internal.Utils        (embedM)

type Config = [SiteConfig]

type SiteConfig = Record
  '[ "title"  >: Text
   , "author" >: Text
   , "url"    >: Text
   , "feed"   >: Maybe Text
   ]

readConfig :: FilePath -> IO (Maybe Config)
readConfig = decodeFile

toSite :: SiteConfig -> Maybe Site
toSite conf = hsequence
    $ #title  <@=> pure (conf ^. #title)
   <: #author <@=> pure (conf ^. #author)
   <: #url    <@=> pure (conf ^. #url)
   <: #id     <@=> toSiteId conf
   <: nil

toSiteId :: SiteConfig -> Maybe SiteId
toSiteId conf
    = embedM (#feed <@=> conf ^. #feed)
  <|> embedM (#url  <@=> pure (conf ^. #url))
