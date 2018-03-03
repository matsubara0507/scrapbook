{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

module ScrapBook.Internal.Utils where

import           Prelude                hiding (takeWhile)

import           Control.Applicative    ((<|>))
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Extensible
import           Data.Maybe             (fromMaybe)
import           Data.Text              (Text, stripPrefix, takeWhile)
import qualified Shelly                 as S

embedM :: (Functor f, x ∈ xs) => Comp f h x -> f (h :| xs)
embedM = fmap embed . getComp

embedAssocM :: (Functor f, Associate k a xs) => Comp f h (k >: a) -> f (h :| xs)
embedAssocM = fmap embedAssoc . getComp

valid :: (a -> Bool) -> a -> Maybe a
valid p a = if p a then pure a else Nothing

sleep :: MonadIO m => Int -> m ()
sleep = S.shelly . S.sleep

toHost :: Text -> Text
toHost url = fromMaybe url
    $ (mappend "https://" . takeWhile (/= '/')) <$> stripPrefix "https://" url
  <|> (mappend "http://"  . takeWhile (/= '/')) <$> stripPrefix "http://"  url
