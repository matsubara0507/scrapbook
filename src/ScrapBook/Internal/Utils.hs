{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

module ScrapBook.Internal.Utils where

import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Extensible
import qualified Shelly                 as S

embedM :: (Functor f, x ∈ xs) => Comp f h x -> f (h :| xs)
embedM = fmap embed . getComp

embedAssocM :: (Functor f, Associate k a xs) => Comp f h (k >: a) -> f (h :| xs)
embedAssocM = fmap embedAssoc . getComp

sleep :: MonadIO m => Int -> m ()
sleep = S.shelly . S.sleep
