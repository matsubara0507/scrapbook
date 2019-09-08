{-# LANGUAGE OverloadedStrings #-}

module Test.ScrapBook.Internal.Utils where

import           RIO

import           ScrapBook.Internal.Utils
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "ScrapBook.Internal.Utils"
  [ testGroup "valid" test_valid
  , testGroup "toHost" test_toHost
  , testGroup "formatTimeFromRFC822" test_formatTimeFromRFC822
  ]


test_valid :: [TestTree]
test_valid =
  [ testCase "valid (const True)"  $ valid (const True)  'a' @?= Just 'a'
  , testCase "valid (const False)" $ valid (const False) 'a' @?= Nothing
  ]

test_toHost :: [TestTree]
test_toHost =
  [ testCase "toHost use Http" $
      toHost "http://example.com/hoge/fuga" @?= "http://example.com"
  , testCase "toHost use Https" $
      toHost "https://example.com/hoge/fuga" @?= "https://example.com"
  , testCase "toHost: no host" $
      toHost "http://" @?= "http://"
  , testCase "toHost: no scheme" $
      toHost "abc" @?= "abc"
  ]

test_formatTimeFromRFC822 :: [TestTree]
test_formatTimeFromRFC822 =
  [ testCase "correct case" $
      formatTimeFromRFC822 "Tue, 06 Mar 2018 05:29:45 GMT" @?= Just "2018-03-06T05:29:45Z"
  , testCase "empty string" $
      formatTimeFromRFC822 "" @?= Nothing
  ]
