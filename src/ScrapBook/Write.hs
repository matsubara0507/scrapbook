{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ScrapBook.Write
    ( write
    , fileName
    , updateFileName
    , Write (..)
    ) where

import           RIO

import           Data.Extensible
import           Data.Proxy               (Proxy (..))
import           Data.Text                (Text)
import           ScrapBook.Collecter      (Collecter)
import           ScrapBook.Data.Config    (Config)
import           ScrapBook.Data.Format    (Format)
import           ScrapBook.Data.Site      (Post)
import           ScrapBook.Feed           ()
import           ScrapBook.Json           ()
import           ScrapBook.Write.Internal (Write (..))

write :: Config -> Format -> [Post] -> Collecter Text
write conf fmt posts = flip matchField fmt $
  htabulateFor (Proxy :: Proxy Write) $
    \m -> Field (Match . pure $ writeTo m conf posts)

fileName :: Config -> Format -> FilePath
fileName conf = matchField $
  htabulateFor (Proxy :: Proxy Write) $
    \m -> Field (Match . pure $ fileName' m conf)

updateFileName :: Format -> FilePath -> Config -> Config
updateFileName = matchField $
  htabulateFor (Proxy :: Proxy Write) $ Field . Match . pure . updateFileName'
