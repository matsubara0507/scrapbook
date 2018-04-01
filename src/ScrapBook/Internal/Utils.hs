{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

module ScrapBook.Internal.Utils where

import           RIO
import qualified RIO.Text               as T
import           RIO.Time

import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Extensible
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
    $ (mappend "https://" . T.takeWhile (/= '/')) <$> T.stripPrefix "https://" url
  <|> (mappend "http://"  . T.takeWhile (/= '/')) <$> T.stripPrefix "http://"  url

formatTimeFromRFC822 :: Text -> Maybe Text
formatTimeFromRFC822 time = formatTimeToRFC3339 <$>
  parseTimeM True defaultTimeLocale rfc822DateFormat (T.unpack time)

formatTimeToRFC3339 :: UTCTime -> Text
formatTimeToRFC3339 =
  T.pack . formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S%EZ")

(<&>) :: Functor f => f a -> (a -> b) -> f b
as <&> f = f <$> as
{-# INLINE (<&>) #-}
infixl 1 <&>
