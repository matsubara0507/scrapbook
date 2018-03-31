{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ScrapBook.Collecter where

import           RIO

import           Data.Extensible
import           Data.Extensible.Effect.Default (EitherDef, runEitherDef)
import           Data.Extensible.Effect.Logger
import           Data.Text                      (Text)
import           Data.Yaml                      (ParseException)
import           Network.HTTP.Req               (HttpException)

type Collecter = Eff
  '[ EitherDef CollectError
   , LoggerDef
   , "IO" >: IO
   ]

data CollectError
  = FetchException (Either HttpException Text)
  | WriteException Text
  | CollectException Text
  | YamlParseException ParseException
  deriving (Show, Eq)

instance Eq HttpException where
  a == b = show a == show b

instance Eq ParseException where
  a == b = show a == show b

collect :: Collecter a -> IO (Either CollectError a)
collect = retractEff . runLoggerDef . runEitherDef
