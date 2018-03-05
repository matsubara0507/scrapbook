{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

import           Paths_scrapbook                 (version)

import           Control.Lens                    (over, (%~), (&), (^.))
import           Control.Monad                   ((<=<))
import           Control.Monad.IO.Class          (liftIO)
import           Data.Drinkery
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
import           System.Directory                (createDirectoryIfMissing)
import           System.FilePath                 (dropFileName,
                                                  replaceExtension,
                                                  takeFileName)
import           System.IO                       (stderr)

main :: IO ()
main = withGetOpt "[options] [input-file]" opts $ \r args ->
  case toCmd (#input @= args <: r) of
    RunScrapBook opts' -> runScrapBook opts'
    PrintVersion       -> putStrLn $ showVersion version
  where
    opts = #output  @= outputOpt
        <: #write   @= writeFormatOpt
        <: #version @= versionOpt
        <: nil

runScrapBook :: Options -> IO ()
runScrapBook opts = runSommelier' (readInputD opts) $&
  traverseFrom_ drinkL (fmap liftIO $ writeOutput' opts <=< run' (opts ^. #write))

drinkL :: (Monoid r, MonadDrunk (Tap r [a]) m) => m [a]
drinkL = drink

readInput :: Options -> IO [Either ParseException Config]
readInput opts = sequence $
    case opts ^. #input of
      []    -> pure $ (decodeEither' . T.encodeUtf8) <$> T.getContents
      paths -> (fmap . fmap . updateFeedName <*> decodeFileEither) <$> paths

readInputD :: Options -> Sommelier () IO (Either ParseException Config)
readInputD = taste <=< liftIO . readInput

writeOutput :: Options -> Config -> Text -> IO ()
writeOutput opts conf txt =
  case opts ^. #output of
    Just dir -> writeFileWithDir (mconcat [dir, "/", name]) txt
    Nothing  -> T.putStrLn txt
  where
    name = fileName conf $ opts ^. #write

writeOutput' :: Options -> Either CollectError (Config, Text) -> IO ()
writeOutput' opts = either terr $ uncurry (writeOutput opts)

showVersion :: Version -> String
showVersion v = unwords
  [ "Version"
  , Version.showVersion v ++ ","
  , "Git revision"
  , $(gitHash)
  , "(" ++ $(gitCommitCount) ++ " commits)"
  ]

terr :: Show e => e -> IO ()
terr err = T.hPutStrLn stderr (pack $ show err)

writeFileWithDir :: FilePath -> Text -> IO ()
writeFileWithDir path txt = do
  createDirectoryIfMissing True $ dropFileName path
  T.writeFile path txt

updateFeedName :: FilePath -> Config -> Config
updateFeedName path conf =
  conf & #feed %~ fmap (over #name $ maybe (pure $ pack name) pure)
  where
    name = replaceExtension (takeFileName path) ".xml"
