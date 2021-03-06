{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module ScrapBook.Cmd.Options where

import           RIO

import           Data.Extensible
import           Data.Extensible.GetOpt
import           ScrapBook.Data.Format

type Options = Record
  '[ "input"   >: [FilePath]
   , "output"  >: Maybe FilePath
   , "write"   >: Format
   , "version" >: Bool
   ]

outputOpt :: OptDescr' (Maybe FilePath)
outputOpt =
  optionReqArg (pure . listToMaybe) ['o'] ["output"]
    "DIR" "Write output to DIR instead of stdout."

writeFormatOpt :: OptDescr' Format
writeFormatOpt =
  optionReqArg (pure . optFormat) ['t','w'] ["to","write"]
    "FORMAT" "Specify output format. default is `feed`."

versionOpt :: OptDescr' Bool
versionOpt = optFlag [] ["version"] "Show version"
