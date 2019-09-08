module Main where

import           RIO
import qualified Test.ScrapBook.Data.Site
import qualified Test.ScrapBook.Internal.Utils

import           Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "scrapbook-core package"
  [ Test.ScrapBook.Data.Site.tests
  , Test.ScrapBook.Internal.Utils.tests
  ]
