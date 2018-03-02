{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module ScrapBook.Cmd.Run
    ( run
    ) where

import           Control.Lens ((^.))
import           Data.Text    (Text)
import           ScrapBook

run :: Format -> Config -> IO (Either CollectError Text)
run fmt conf = collect $
  (write conf fmt . concat) =<< mapM (fetch . toSite) (conf ^. #sites)
