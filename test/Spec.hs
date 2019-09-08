module Main where

import           RIO
import qualified Test.ScrapBook.Cmd

import           Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "scrapbook package"
  [ Test.ScrapBook.Cmd.tests
  ]
