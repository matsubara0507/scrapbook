{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module ScrapBook.Write
    ( write
    , fileName
    , updateFileName
    , Write (..)
    ) where

import           RIO

import           Data.Extensible
import           Data.Proxy               (Proxy (..))
import           ScrapBook.Collecter      (Collecter)
import           ScrapBook.Data.Config    (HasWriteConfigFields)
import           ScrapBook.Data.Format    (Format)
import           ScrapBook.Data.Site      (IsSiteFields, Post)
import           ScrapBook.Feed           ()
import           ScrapBook.Json           ()
import           ScrapBook.Write.Internal (Write (..))

write :: (IsSiteFields xs, HasWriteConfigFields ys) =>
  Record ys -> Format -> [Post (Record xs)] -> Collecter Text
write conf fmt posts = flip matchField fmt $
  htabulateFor (Proxy :: Proxy Write) $
    \m -> Field (Match . pure $ writeTo m conf posts)

fileName :: HasWriteConfigFields xs => Record xs -> Format -> FilePath
fileName conf = matchField $
  htabulateFor (Proxy :: Proxy Write) $
    \m -> Field (Match . pure $ fileName' m conf)

updateFileName :: HasWriteConfigFields xs =>
  Format -> FilePath -> Record xs -> Record xs
updateFileName = matchField $
  htabulateFor (Proxy :: Proxy Write) $ Field . Match . pure . updateFileName'
