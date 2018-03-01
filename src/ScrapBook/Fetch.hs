{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module ScrapBook.Fetch where

import           Control.Lens             ((^.))
import           Data.Extensible
import           Data.Functor.Identity
import           Data.Proxy               (Proxy (..))
import           Data.Text                (Text)
import           ScrapBook.Collecter
import           ScrapBook.Data.Site
import           ScrapBook.Feed           ()
import           ScrapBook.Fetch.Internal (Fetch (..))

fetch :: Site -> Collecter [Post]
fetch site = flip matchField (site ^. #id) $
  htabulateFor (Proxy :: Proxy Fetch) $
    \m -> Field (Match $ fetchFrom m site . runIdentity)

instance Fetch ("url" >: Text) where
  fetchFrom _ _ _ = pure []