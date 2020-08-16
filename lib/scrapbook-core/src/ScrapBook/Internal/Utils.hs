{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

module ScrapBook.Internal.Utils where

import           RIO
import qualified RIO.Text        as T
import           RIO.Time

import           Data.Extensible

embedM :: (Functor f, x ∈ xs) => Comp f h x -> f (xs :/ h)
embedM = fmap embed . getComp

embedAssocM :: (Functor f, Lookup xs k a) => Comp f h (k >: a) -> f (xs :/ h)
embedAssocM = fmap embedAssoc . getComp

valid :: (a -> Bool) -> a -> Maybe a
valid p a = if p a then pure a else Nothing

toHost :: Text -> Text
toHost url = fromMaybe url
    $ mappend "https://" . T.takeWhile (/= '/') <$> T.stripPrefix "https://" url
  <|> mappend "http://"  . T.takeWhile (/= '/') <$> T.stripPrefix "http://"  url

formatTimeFromRFC822 :: Text -> Maybe Text
formatTimeFromRFC822 time = formatTimeToRFC3339 <$>
  parseTimeM True defaultTimeLocale rfc822DateFormat (T.unpack time)

formatTimeToRFC3339 :: UTCTime -> Text
formatTimeToRFC3339 =
  T.pack . formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S%Ez")
