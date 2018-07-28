{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

module ScrapBook.Data.Config where

import           RIO
import qualified RIO.Text                 as T

import           Data.Extensible
import           Data.Yaml
import           ScrapBook.Data.Site
import           ScrapBook.Internal.Utils (embedM)

type Config = Record
  '[ "feed"  >: Maybe FeedConfig
   , "json"  >: Maybe Text        -- ^ output file name
   , "sites" >: [SiteConfig]
   ]

type HasWriteConfigFields xs =
  ( Associate "feed" (Maybe FeedConfig) xs
  , Associate "json" (Maybe Text) xs
  )

type FeedConfig = Record
  '[ "title"   >: Text
   , "baseUrl" >: Text
   , "name"    >: Maybe Text      -- ^ output file name
   ]

type SiteConfig = Record
  '[ "title"  >: Text
   , "author" >: Text
   , "url"    >: Text
   , "feed"   >: Maybe Text
   , "atom"   >: Maybe AtomConfig
   , "rss"    >: Maybe Text
   ]

readConfig :: FilePath -> IO (Maybe Config)
readConfig = fmap (either (const Nothing) pure) . decodeFileEither

toSite :: SiteConfig -> Site
toSite conf
    = #title  @= (conf ^. #title)
   <: #author @= (conf ^. #author)
   <: #url    @= (conf ^. #url)
   <: #id     @= toSiteId conf
   <: nil

toSiteId :: SiteConfig -> SiteId
toSiteId conf = fromMaybe (embed $ #url @= conf ^. #url)
    $ embedM (#atom <@=> conf ^. #atom)
  <|> embedM (#rss  <@=> conf ^. #rss)
  <|> embedM (#feed <@=> conf ^. #feed)

feedName :: FeedConfig -> FilePath
feedName conf = maybe "atom.xml" T.unpack (conf ^. #name)
