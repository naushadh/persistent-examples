{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- NFdata instance

module Main where

import Gauge.Main
import qualified Database.PersistExample.Stream as Stream
import qualified Database.Persist.Postgresql as P
import           Data.Pool (Pool)
import           Control.Monad.Logger (runNoLoggingT, NoLoggingT)
import           Control.Monad.Trans.Control (MonadBaseControl)
import qualified Control.DeepSeq as DeepSeq
import           Control.Monad.IO.Unlift (MonadUnliftIO)

main :: IO ()
main
  = defaultMain
  [ bgroup "MyRecord/Looped"
      [ bench "10" . benchInEnv $ Stream.myRecordLooped 10
      , bench "100" . benchInEnv $ Stream.myRecordLooped 100
      , bench "1000" . benchInEnv $ Stream.myRecordLooped 1000
      ]
  , bgroup "MyRecord/Batch"
      [ bench "10" . benchInEnv $ Stream.myRecordBatch 10
      , bench "100" . benchInEnv $ Stream.myRecordBatch 100
      , bench "1000" . benchInEnv $ Stream.myRecordBatch 1000
      ]
  , bgroup "MyUniqueRecord/Looped"
      [ bench "10" . benchInEnv $ Stream.myUniqueRecordLooped 10
      , bench "100" . benchInEnv $ Stream.myUniqueRecordLooped 100
      , bench "1000" . benchInEnv $ Stream.myUniqueRecordLooped 1000
      ]
  , bgroup "MyUniqueRecord/Batch"
      [ bench "10" . benchInEnv $ Stream.myUniqueRecordBatch 10
      , bench "100" . benchInEnv $ Stream.myUniqueRecordBatch 100
      , bench "1000" . benchInEnv $ Stream.myUniqueRecordBatch 1000
      ]
  ]

benchInEnv
  :: DeepSeq.NFData b
  => Stream.ReaderT P.SqlBackend (NoLoggingT IO) b
  -> Benchmarkable
benchInEnv b = perRunEnvWithCleanup mkDb cleanDB (flip runDB $ b)

instance DeepSeq.NFData (Pool P.SqlBackend) where
  rnf x = x `seq` ()

runDB
  ::( P.BaseBackend backend ~ P.SqlBackend
    , P.IsPersistBackend backend
    , MonadBaseControl IO m
    , MonadUnliftIO m
    )
  => Pool backend
  -> Stream.ReaderT backend (NoLoggingT m) a
  -> m a
runDB dbPool m = runNoLoggingT $ P.runSqlPool m dbPool

mkDb :: IO (Pool P.SqlBackend)
mkDb = do
  dbPool <- runNoLoggingT $ P.createPostgresqlPool ci 1
  runDB dbPool $ P.runMigration Stream.migrateAll
  return dbPool
  where
    ci = "host=localhost dbname=batchingvsloop user=postgres"

cleanDB :: Pool P.SqlBackend -> IO ()
cleanDB dbPool
  = runDB dbPool
  $ do
    P.deleteWhere ([] :: [P.Filter Stream.MyRecord])
    P.deleteWhere ([] :: [P.Filter Stream.MyUniqueRecord])