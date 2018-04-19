{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ScrapBook.Internal.Instances where

import           RIO
import qualified RIO.HashMap           as M.Hash
import qualified RIO.Text              as T

import           Data.Aeson            (ToJSON (..))
import qualified Data.Aeson            as JSON
import           Data.Extensible
import           Data.Functor.Identity
import           Data.Proxy            (Proxy (..))
import           GHC.TypeLits          (KnownSymbol, symbolVal)

instance Forall (KeyValue KnownSymbol ToJSON) xs => ToJSON (Variant xs) where
  toJSON = matchField $
    htabulateFor (Proxy :: Proxy (KeyValue KnownSymbol ToJSON)) $ \m ->
      let key = T.pack . symbolVal $ proxyAssocKey m
      in Field (Match $ JSON.Object . kvToJSON key . runIdentity)

kvToJSON :: ToJSON v => Text -> v -> JSON.Object
kvToJSON key val =
  M.Hash.fromList [("type", JSON.String key), ("value", toJSON val)]
