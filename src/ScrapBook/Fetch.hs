{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ScrapBook.Fetch
  ( fetch
  , Fetch (..)
  ) where

import           RIO

import           Control.Monad.Error.Class (throwError)
import           Data.Extensible
import           Data.Functor.Identity
import           Data.Proxy                (Proxy (..))
import           ScrapBook.Collecter
import           ScrapBook.Data.Site
import           ScrapBook.Feed            ()
import           ScrapBook.Fetch.Internal  (Fetch (..))

fetch :: Site -> Collecter [Post]
fetch site = flip matchField (site ^. #id) $
  htabulateFor (Proxy :: Proxy Fetch) $
    \m -> Field (Match $ fetchFrom m site . runIdentity)

instance Fetch ("url" >: Text) where
  fetchFrom _ _ _ = throwError $ CollectException "undefined fetching behavior."
