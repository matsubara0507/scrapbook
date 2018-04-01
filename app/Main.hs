{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

import           Paths_scrapbook        (version)
import           RIO
import           RIO.Directory
import           RIO.FilePath
import qualified RIO.Text               as T

import           Data.Drinkery
import           Data.Extensible
import           Data.Extensible.GetOpt
import qualified Data.Text.IO           as T
import           Data.Version           (Version)
import qualified Data.Version           as Version
import           Data.Yaml              (ParseException, decodeEither',
                                         decodeFileEither)
import           Development.GitRev
import           ScrapBook
import           ScrapBook.Cmd

main :: IO ()
main = withGetOpt "[options] [input-file]" opts $ \r args ->
  case toCmd (#input @= args <: r) of
    RunScrapBook opts' -> runScrapBook opts'
    PrintVersion       -> T.putStrLn $ T.pack (showVersion version)
  where
    opts = #output  @= outputOpt
        <: #write   @= writeFormatOpt
        <: #version @= versionOpt
        <: nil

runScrapBook :: Options -> IO ()
runScrapBook opts = tapListT (readInputD opts) $&
  traverseFrom_ consumeL (fmap liftIO $ writeOutput' opts <=< run' (opts ^. #write))

consumeL :: (Monoid r, MonadSink (Tap r [a]) m) => m [a]
consumeL = consume

readInput :: Options -> IO [Either ParseException Config]
readInput opts = sequence $
    case opts ^. #input of
      []    -> pure $ (decodeEither' . T.encodeUtf8) <$> T.getContents
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
    Nothing  -> T.putStrLn txt
  where
    name = fileName conf $ opts ^. #write

writeOutput' :: Options -> (Config, Text) -> IO ()
writeOutput' opts = handle terr . uncurry (writeOutput opts)

showVersion :: Version -> String
showVersion v = unwords
  [ "Version"
  , Version.showVersion v ++ ","
  , "Git revision"
  , $(gitHash)
  , "(" ++ $(gitCommitCount) ++ " commits)"
  ]

terr :: CollectError -> IO ()
terr err = T.hPutStrLn stderr (tshow err)

writeFileWithDir :: FilePath -> Text -> IO ()
writeFileWithDir path txt = do
  createDirectoryIfMissing True $ dropFileName path
  T.writeFile path txt
