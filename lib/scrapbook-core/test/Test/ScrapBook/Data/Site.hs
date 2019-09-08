{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.ScrapBook.Data.Site where

import           RIO

import           Data.Extensible
import           ScrapBook.Data.Site
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "ScrapBook.Data.Site"
  [ testGroup "toAbsoluteUrl" test_toAbsoluteUrl
  ]

site :: Site
site
    = #title @= "Hoge Site"
   <: #author @= "hoge"
   <: #url @= "https://example.com"
   <: #id @= embedAssoc (#feed @= "https://example.com/feed")
   <: nil

test_toAbsoluteUrl :: [TestTree]
test_toAbsoluteUrl =
  [ testCase "prefix is `/`" $
      toAbsoluteUrl site "/aaa/bbb.html" @?= "https://example.com/aaa/bbb.html"
  , testCase "prefix is not `/`" $
      toAbsoluteUrl site "https://example.com/aaa/bbb.html" @?= "https://example.com/aaa/bbb.html"
  , testCase "empty string" $
      toAbsoluteUrl site "" @?= ""
  ]
