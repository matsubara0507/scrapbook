{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ScrapBook.Fetch
  ( fetch
  , Fetch (..)
  ) where

import           RIO

import           Data.Extensible
import           ScrapBook.Collecter
import           ScrapBook.Data.Site
import           ScrapBook.Feed           ()
import           ScrapBook.Fetch.Internal (Fetch (..))

fetch :: IsSiteFields xs => Record xs -> Collecter [Post (Record xs)]
fetch site = flip matchField (site ^. #id) $
  htabulateFor (Proxy :: Proxy Fetch) $
    \m -> Field (Match $ fetchFrom m site . runIdentity)

instance Fetch ("url" >: Text) where
  fetchFrom _ _ _ = throwM $ CollectException "undefined fetching behavior."
