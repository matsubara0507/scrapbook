{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module ScrapBook.Cmd.Options where

import           Data.Extensible
import           Data.Extensible.GetOpt
import           Data.Maybe             (listToMaybe)
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
    "FORMAT" "Specify output format. default is `markdown`."

versionOpt :: OptDescr' Bool
versionOpt = optFlag [] ["version"] "Show version"
