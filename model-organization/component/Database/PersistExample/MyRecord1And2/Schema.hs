{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Database.PersistExample.MyRecord1And2.Schema where

import qualified Database.Persist.TH as TH
import qualified Database.Persist.Quasi as PQ
import           Data.Time (UTCTime)
import           Database.PersistExample.MyRecord1.Schema (MyRecord1Id)
import           Database.PersistExample.MyRecord2.Schema (MyRecord2Id)

TH.share
  [TH.mkPersist TH.sqlSettings]
  $(TH.persistFileWith PQ.lowerCaseSettings "Database/PersistExample/MyRecord1And2/Models")