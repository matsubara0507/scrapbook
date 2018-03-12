{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}

module Test.ScrapBook.Cmd where

import           Control.Lens           ((&), (.~))
import           Data.Extensible
import           Data.Extensible.GetOpt
import           ScrapBook.Cmd
import           Test.Tasty
import           Test.Tasty.HUnit

opts :: Options
opts
    = #input   @= ["sites.yaml"]
   <: #output  @= Just "example"
   <: #write   @= embedAssoc (#feed @= ())
   <: #version @= False
   <: nil

getOptRecord' ::
  RecordOf (OptionDescr h) xs
  -> [String]
  -> Either [String] (RecordOf h xs, [String])
getOptRecord' r args = getOptRecord r args & \case
  (result, rs, [], _) -> Right (result, rs)
  (_, _, errs, _)     -> Left errs

test_toCmd :: [TestTree]
test_toCmd =
  [ testCase "default optsion" $
      toCmd opts @?= RunScrapBook opts
  , testCase "#version field is True" $
      toCmd (opts & #version .~ True) @?= PrintVersion
  ]

test_optParser :: [TestTree]
test_optParser =
  [ testCase "correct case: basic" $
      getOptRecord' parser ["-o", "example", "sites.yaml"] @?=
        Right (shrink opts, ["sites.yaml"])
  , testCase "correct case: version" $
      getOptRecord' parser ["--version", "-o", "example", "sites.yaml"] @?=
        Right (shrink (opts & #version .~ True) , ["sites.yaml"])
  , testCase "correct case: no output" $
      getOptRecord' parser ["sites.yaml"] @?=
        Right (shrink (opts & #output .~ Nothing) , ["sites.yaml"])
  , testCase "correct case: no arguments" $
      getOptRecord' parser [] @?=
        Right (shrink (opts & #output .~ Nothing) , [])
  , testCase "correct case: feed format " $
      getOptRecord' parser ["-o", "example", "-t", "feed", "sites.yaml"] @?=
        Right (shrink opts , ["sites.yaml"])
  , testCase "correct case: json format " $
      getOptRecord' parser ["-o", "example", "-t", "json", "sites.yaml"] @?=
        Right (shrink (opts & #write .~ embedAssoc (#json @= ())) , ["sites.yaml"])
  , testCase "incorrect case: unkown ooption" $
      getOptRecord' parser ["-h", "-o", "example", "sites.yaml"] @?=
        Left ["unrecognized option `-h'\n"]
  ]
  where
    parser
        = #output  @= outputOpt
       <: #write   @= writeFormatOpt
       <: #version @= versionOpt
       <: nil
