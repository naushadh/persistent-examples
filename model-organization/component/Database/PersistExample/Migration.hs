{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Database.PersistExample.Migration (migrateAll) where

import qualified Database.Persist.Quasi as PQ
import qualified Database.Persist.TH as PTH
import           Data.Time (UTCTime)
import           Data.Text (Text)
import           Database.PersistExample.Util (persistManyFileWith)

PTH.share
  [PTH.mkMigrate "migrateAll"]
  $(persistManyFileWith PQ.lowerCaseSettings
    [ "Database/PersistExample/MyRecord1/Models"
    , "Database/PersistExample/MyRecord2/Models"
    , "Database/PersistExample/MyRecord3/Models"
    , "Database/PersistExample/MyRecord1And2/Models"
    ])