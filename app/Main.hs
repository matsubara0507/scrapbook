{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Paths_scrapbook                 (version)

import           Control.Lens                    ((^.))
import           Data.Extensible
import           Data.Extensible.GetOpt
import           Data.Extensible.Instances.Aeson ()
import           Data.Text                       (Text, pack)
import qualified Data.Text.Encoding              as T
import qualified Data.Text.IO                    as T
import           Data.Version                    (Version)
import qualified Data.Version                    as Version
import           Data.Yaml                       (ParseException, decodeEither',
                                                  decodeFileEither)
import           Development.GitRev
import           ScrapBook
import           ScrapBook.Cmd
import           System.IO                       (stderr)

main :: IO ()
main = withGetOpt "[options] [input-file]" opts $ \r args -> do
  case toCmd (#input @= toInput args <: r) of
    RunScrapBook opts' -> runScrapBook opts'
    PrintVersion       -> putStrLn $ showVersion version
  where
    opts = #output  @= outputOpt
        <: #write   @= writeFormatOpt
        <: #version @= versionOpt
        <: nil

runScrapBook :: Options -> IO ()
runScrapBook opts = do
  config <- readInput opts
  case config of
    Left err  -> T.hPutStrLn stderr (pack $ show err)
    Right conf -> do
      result <- run (opts ^. #write) conf
      case result of
        Right txt -> writeOutput opts conf txt
        Left err  -> T.hPutStrLn stderr (pack $ show err)

readInput :: Options -> IO (Either ParseException Config)
readInput opt =
  case opt ^. #input of
    Just path -> decodeFileEither path
    Nothing   -> (decodeEither' . T.encodeUtf8) <$> T.getContents

writeOutput :: Options -> Config -> Text -> IO ()
writeOutput opts conf txt =
  case opts ^. #output of
    Just dir -> T.writeFile (mconcat [dir, "/", name]) txt
    Nothing  -> T.putStrLn txt
  where
    name = fileName conf $ opts ^. #write

showVersion :: Version -> String
showVersion v = unwords
  [ "Version"
  , Version.showVersion v ++ ","
  , "Git revision"
  , $(gitHash)
  , "(" ++ $(gitCommitCount) ++ " commits)"
  ]
