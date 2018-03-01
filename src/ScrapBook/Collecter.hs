{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ScrapBook.Collecter where

import           Data.Extensible
import           Data.Extensible.Effect.Default (EitherDef, runEitherDef)
import           Data.Extensible.Effect.Logger
import           Data.Text                      (Text)
import           Network.HTTP.Req               (HttpException)

type Collecter = Eff
  '[ EitherDef CollectError
   , LoggerDef
   , "IO" >: IO
   ]

data CollectError
  = FetchException (Either HttpException Text)
  | CollectException Text
  deriving (Show, Eq)

instance Eq HttpException where
  a == b = show a == show b

collect :: Collecter a -> IO (Either CollectError a)
collect = retractEff . runLoggerDef . runEitherDef
