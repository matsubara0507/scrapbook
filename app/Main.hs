{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

import           Paths_scrapbook        (version)
import           RIO
import qualified RIO.ByteString         as B
import           RIO.Directory
import           RIO.FilePath

import           Data.Drinkery
import           Data.Extensible
import           Data.Extensible.GetOpt
import           Data.Version           (Version)
import qualified Data.Version           as Version
import           Data.Yaml              (ParseException, decodeEither',
                                         decodeFileEither)
import qualified GitHash
import           ScrapBook
import           ScrapBook.Cmd

main :: IO ()
main = withGetOpt "[options] [input-file]" opts $ \r args ->
  case toCmd (#input @= args <: r) of
    RunScrapBook opts' -> runScrapBook opts'
    PrintVersion       -> B.putStr $ fromString (showVersion version)
  where
    opts = #output  @= outputOpt
        <: #write   @= writeFormatOpt
        <: #version @= versionOpt
        <: nil

runScrapBook :: Options -> IO ()
runScrapBook opts = tapListT (readInputD opts) $&
  traverseFrom_ consume (fmap liftIO $ writeOutput' opts <=< run' (opts ^. #write))

readInput :: Options -> IO [Either ParseException Config]
readInput opts = sequence $
    case opts ^. #input of
      []    -> pure $ decodeEither' <$> B.getContents
      paths -> decodeFileEither' <$> paths
  where
    decodeFileEither' path =
      fmap (updateFileName (opts ^. #write) path) <$> decodeFileEither path

readInputD :: Options -> ListT () IO (Either ParseException Config)
readInputD = sample <=< liftIO . readInput

writeOutput :: Options -> Config -> Text -> IO ()
writeOutput opts conf txt =
  case opts ^. #output of
    Just dir -> writeFileWithDir (mconcat [dir, "/", name]) txt
    Nothing  -> hPutBuilder stdin $ encodeUtf8Builder txt
  where
    name = fileName conf $ opts ^. #write

writeOutput' :: Options -> (Config, Text) -> IO ()
writeOutput' opts = handle terr . uncurry (writeOutput opts)

showVersion :: Version -> String
showVersion v = unwords
  [ "Version"
  , Version.showVersion v ++ ","
  , "Git revision"
  , GitHash.giHash gi
  , "(" ++ show (GitHash.giCommitCount gi) ++ " commits)"
  ]
  where
    gi = $$(GitHash.tGitInfoCwd)

terr :: CollectError -> IO ()
terr err = hPutBuilder stderr $ encodeUtf8Builder (tshow err)

writeFileWithDir :: FilePath -> Text -> IO ()
writeFileWithDir path txt = do
  createDirectoryIfMissing True $ dropFileName path
  writeFileUtf8 path txt
