{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ScrapBook.Internal.Instances where

import           RIO

import           Data.Aeson      (ToJSON (..))
import qualified Data.Aeson      as JSON
import           GHC.Exts        (fromList)
import           Data.Extensible

instance Forall (KeyTargetAre KnownSymbol ToJSON) xs => ToJSON (Variant xs) where
  toJSON = matchField $
    htabulateFor (Proxy :: Proxy (KeyTargetAre KnownSymbol ToJSON)) $ \m ->
      Field (Match $ JSON.Object . kvToJSON (stringKeyOf m) . runIdentity)

kvToJSON :: ToJSON v => Text -> v -> JSON.Object
kvToJSON key val =
  fromList [("type", JSON.String key), ("value", toJSON val)]
