{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE TypeOperators    #-}

module ScrapBook.Data.Format
  ( Format
  , optFormat
  ) where

import           RIO

import           Control.Applicative      ((<|>))
import           Data.Default
import           Data.Extensible
import           Data.Maybe               (fromMaybe, listToMaybe)
import           Data.Proxy               (Proxy (..))
import           GHC.TypeLits
import           ScrapBook.Internal.Utils (valid)

type Format = Variant FormatFields

type FormatFields =
  '[ "feed" >: ()
   , "json" >: ()
   ]

optFormat :: [String] -> Format
optFormat args = fromMaybe feed' (gen Nothing)
  where
    gen = henumerateFor
      (Proxy :: Proxy (KeyValue KnownSymbol Default))
      (Proxy :: Proxy FormatFields)
      $ \m r ->
        let key = symbolVal $ proxyAssocKey m in
        let val = EmbedAt m $ Field (pure def) in
        r <|> (fmap (const val) . valid (key ==) =<< listToMaybe args)

feed' :: Format
feed' = embedAssoc $ #feed @= ()
