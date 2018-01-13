{-# LANGUAGE OverloadedStrings #-}

module Database.PersistExample.Stream (
    createDB
  , doList
  , doStream
) where

import Data.Conduit (await,yield,leftover,Conduit,Sink,($$),(=$))
import qualified Data.Conduit.List as CL
import Database.Persist (insertMany_,selectList,entityVal,Entity, selectSource)
import Database.Persist.Sql (runMigration, SqlPersistT)
import Control.Monad (mapM_)
import Control.Monad.IO.Class  (liftIO, MonadIO)
import qualified Database.PersistExample.Schema as S
import Control.Monad.Trans.Resource.Internal (MonadResource)

-- ----------------------------------------------------------------------------
-- * SEED

-- | Create test DB and populate with values
createDB :: MonadIO m => SqlPersistT m ()
createDB = do
  let n = 1000000
  runMigration S.migrateAll
  insertMany_ $ map S.MyRecord [1..n]

  return ()

-- ----------------------------------------------------------------------------
-- * LIST

doList :: MonadIO m => SqlPersistT m ()
doList = do
  records <- selectList [] []
  let values = map (S.myRecordValue . entityVal) records
  mapM_ (liftIO . putStrLn . show) $ zip values $ tail values

-- ----------------------------------------------------------------------------
-- * STREAM

doStream :: MonadResource m => SqlPersistT m ()
doStream = selectSource [] [] $$ entityToValue =$ showPairs =$ printString

-- |Unpacks incoming values from upstream from MyRecord to Int
entityToValue :: MonadIO m => Conduit (Entity S.MyRecord) m Int
entityToValue = CL.map (S.myRecordValue . entityVal)

-- |Converts pairwise tuples of Ints into String
showPairs :: MonadIO m => Conduit Int m String
showPairs = do
  mi1 <- await -- get the next value from the input stream
  mi2 <- await
  case (mi1, mi2) of
    (Just i1, Just i2) -> do
      yield $ show (i1, i2) -- pass tuple of Ints converted
                            -- to String downstream
      leftover i2           -- pass the second component of
                            -- the tuple back to itself (to
                            -- the upstream)
      showPairs
    _ -> return ()

-- |Prints input String
printString :: (Monad m, MonadIO m) => Sink String m ()
printString = CL.mapM_ (liftIO . putStrLn)