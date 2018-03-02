{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module ScrapBook.Cmd.Options where

import           Data.Extensible
import           Data.Extensible.GetOpt
import           Data.Maybe             (listToMaybe)
import           ScrapBook.Data.Format

type Options = Record
  '[ "input" >: Maybe FilePath
   , "output" >: Maybe FilePath
   , "write"  >: Format
   ]

toInput :: [String] -> Maybe FilePath
toInput = listToMaybe

outputOpt :: OptDescr' (Maybe FilePath)
outputOpt =
  optionReqArg (pure . listToMaybe) ['o'] ["output"]
    "DIR" "Write output to DIR instead of stdout."

writeFormatOpt :: OptDescr' Format
writeFormatOpt =
  optionReqArg (pure . optFormat) ['t','w'] ["to","write"]
    "FORMAT" "Specify output format. default is `markdown`."
