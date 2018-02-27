{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Database.PersistExample.MyRecord18.Schema where

import qualified Database.Persist.TH as TH
import qualified Database.Persist.Quasi as PQ
import           Data.Time (UTCTime)
import           Data.Text (Text)

TH.share
  [TH.mkPersist TH.sqlSettings]
  $(TH.persistFileWith PQ.lowerCaseSettings "Database/PersistExample/MyRecord18/Models")