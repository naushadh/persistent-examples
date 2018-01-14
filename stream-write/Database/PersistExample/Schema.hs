{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-} -- Arbitrary Key instance.

module Database.PersistExample.Schema where

import qualified Database.Persist.Sql as P
import qualified Database.Persist.TH as TH
import           Data.Text (Text)
import qualified Test.QuickCheck.Arbitrary as QC
import           Test.QuickCheck.Instances () -- provides 'text' instances.

TH.share [TH.mkPersist TH.sqlSettings, TH.mkMigrate "migrateAll"] [TH.persistLowerCase|
MyRecord
  col1 Text
  col2 Int
  col3 Bool
  col4 Text Maybe
  col5 Int Maybe
  col6 Bool Maybe
  deriving Show

MyUniqueRecord
  col7 Text
  col8 Int
  col9 Bool
  col10 Text Maybe
  col11 Int Maybe
  col12 Bool Maybe
  UniqueMUR col7
  deriving Show
|]

-- | generate test record for convenience
instance QC.Arbitrary MyRecord where
  arbitrary = MyRecord
          <$> QC.arbitrary
          <*> QC.arbitrary
          <*> QC.arbitrary
          <*> QC.arbitrary
          <*> QC.arbitrary
          <*> QC.arbitrary

-- | generate test record for convenience
instance QC.Arbitrary MyUniqueRecord where
  arbitrary = MyUniqueRecord
          <$> QC.arbitrary
          <*> QC.arbitrary
          <*> QC.arbitrary
          <*> QC.arbitrary
          <*> QC.arbitrary
          <*> QC.arbitrary

-- | generate test keys for convenience
instance P.ToBackendKey P.SqlBackend r => QC.Arbitrary (P.Key r) where
  arbitrary = P.toSqlKey <$> QC.arbitrary

-- | generate test entities for convenience
instance (P.ToBackendKey P.SqlBackend r, QC.Arbitrary r) => QC.Arbitrary (P.Entity r) where
  arbitrary = P.Entity <$> QC.arbitrary <*> QC.arbitrary