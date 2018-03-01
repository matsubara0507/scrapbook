{-# LANGUAGE PolyKinds #-}

module ScrapBook.Fetch.Internal
  ( Fetch (..)
  ) where

import           Data.Extensible
import           Data.Proxy          (Proxy (..))
import           Data.Text           (Text)
import           ScrapBook.Collecter (Collecter)
import           ScrapBook.Data.Site (Post, Site)

class Fetch kv where
  fetchFrom :: proxy kv -> Site -> AssocValue kv -> Collecter [Post]
