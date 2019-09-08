{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE TypeOperators    #-}

module ScrapBook.Data.Format
  ( Format
  , optFormat
  ) where

import           RIO

import           Data.Extensible
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
      (Proxy :: Proxy (KeyTargetAre KnownSymbol Monoid))
      (Proxy :: Proxy FormatFields)
      $ \m r ->
        let key = stringKeyOf m in
        let val = EmbedAt m $ Field (pure mempty) in
        r <|> (fmap (const val) . valid (key ==) =<< listToMaybe args)

feed' :: Format
feed' = embedAssoc $ #feed @= ()
