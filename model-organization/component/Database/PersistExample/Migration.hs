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
-- import           Data.Time (UTCTime)
-- import           Data.Text (Text)
import           Database.PersistExample.Util (persistManyFileWith)

PTH.share
  [PTH.mkMigrate "migrateAll"]
  $(persistManyFileWith PQ.lowerCaseSettings []
    -- [ "Database/PersistExample/MyRecord1/Models"
    -- , "Database/PersistExample/MyRecord2/Models"
    -- , "Database/PersistExample/MyRecord3/Models"
    -- , "Database/PersistExample/MyRecord1And2/Models"
    -- , "Database/PersistExample/MyRecord4/Models"
    -- , "Database/PersistExample/MyRecord5/Models"
    -- , "Database/PersistExample/MyRecord6/Models"
    -- , "Database/PersistExample/MyRecord7/Models"
    -- , "Database/PersistExample/MyRecord8/Models"
    -- , "Database/PersistExample/MyRecord9/Models"
    -- , "Database/PersistExample/MyRecord10/Models"
    -- , "Database/PersistExample/MyRecord11/Models"
    -- , "Database/PersistExample/MyRecord12/Models"
    -- , "Database/PersistExample/MyRecord13/Models"
    -- , "Database/PersistExample/MyRecord14/Models"
    -- , "Database/PersistExample/MyRecord15/Models"
    -- , "Database/PersistExample/MyRecord16/Models"
    -- , "Database/PersistExample/MyRecord17/Models"
    -- , "Database/PersistExample/MyRecord18/Models"
    -- , "Database/PersistExample/MyRecord19/Models"
    -- , "Database/PersistExample/MyRecord20/Models"
    -- ]
    )