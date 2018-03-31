{-# LANGUAGE OverloadedLabels #-}

module ScrapBook.Cmd
    ( module X
    , Cmd (..)
    , toCmd
    ) where

import           RIO

import           ScrapBook.Cmd.Options as X
import           ScrapBook.Cmd.Run     as X

data Cmd
  = RunScrapBook Options
  | PrintVersion
  deriving (Show, Eq)

toCmd :: Options -> Cmd
toCmd opts
  | opts ^. #version = PrintVersion
  | otherwise        = RunScrapBook opts
