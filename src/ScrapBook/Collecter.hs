{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module ScrapBook.Collecter where

import           Data.Extensible
import           Data.Extensible.Effect.Default (EitherDef, runEitherDef)
import           Data.Extensible.Effect.Logger
import           Data.Text                      (Text)
import           ScrapBook.Data.Site

type Collecter = Eff
  '[ EitherDef CollectError
   , LoggerDef
   , "IO" >: IO
   ]

type CollectError = Text

collect :: Collecter a -> IO (Either CollectError a)
collect = retractEff . runLoggerDef . runEitherDef
