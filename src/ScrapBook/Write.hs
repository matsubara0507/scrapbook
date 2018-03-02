{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ScrapBook.Write
    ( write
    , Write (..)
    ) where


import           Data.Extensible
import           Data.Proxy               (Proxy (..))
import           Data.Text                (Text)
import           ScrapBook.Collecter      (Collecter)
import           ScrapBook.Data.Config    (Config)
import           ScrapBook.Data.Format    (Format)
import           ScrapBook.Data.Site      (Post)
import           ScrapBook.Feed           ()
import           ScrapBook.Write.Internal (Write (..), throwWriteError)

write :: Config -> Format -> [Post] -> Collecter Text
write conf fmt posts = flip matchField fmt $
  htabulateFor (Proxy :: Proxy Write) $
    \m -> Field (Match . pure $ writeTo m conf posts)

instance Write ("json" >: ()) where
  writeTo _ _conf _posts = throwWriteError "undefined write to json."
