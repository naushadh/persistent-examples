{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Database.PersistExample.Stream (
    Schema.migrateAll
  , Schema.MyRecord
  , Schema.MyUniqueRecord
  , ReaderT
  , myRecordLooped
  , myRecordBatch
  , myUniqueRecordLooped
  , myUniqueRecordBatch
) where

import qualified Database.PersistExample.Schema as Schema
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import           Data.Conduit ((.|), ($$))
import qualified Test.QuickCheck as QC
import qualified Database.Persist.Sql as P
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Reader (ReaderT)
import qualified Data.Time as Time
import qualified Data.Text as T

-------------------------------------------------------------------------------
-- * SOURCE

-- | source a stream of values.
sourceGen
  :: (QC.Arbitrary c, MonadIO m)
  => (c -> IO c)
  -> Int
  -> C.Source m c
sourceGen f n
  =   CL.sourceList [1..n]
  .|  CL.mapM (const . liftIO . (\c -> c >>= f) . QC.generate $ QC.arbitrary)

-------------------------------------------------------------------------------
-- * SINKS

myRecordLooped
  ::( P.BaseBackend backend ~ P.SqlBackend
    , P.PersistStoreWrite backend
    , MonadIO m
    )
  => Int
  -> ReaderT backend m ()
myRecordLooped n = sourceGen f n $$ mkLoopedSink repsert
  where
    f :: P.Entity Schema.MyRecord -> IO (P.Entity Schema.MyRecord)
    f = return

myRecordBatch
  ::( P.BaseBackend backend ~ P.SqlBackend
    , P.PersistStoreWrite backend
    , MonadIO m
    )
  => Int
  -> ReaderT backend m ()
myRecordBatch n = sourceGen f n $$ mkBatchSink repsertMany
  where
    f :: P.Entity Schema.MyRecord -> IO (P.Entity Schema.MyRecord)
    f = return

myUniqueRecordLooped
  ::( P.BaseBackend backend ~ P.SqlBackend
    , P.PersistStoreWrite backend
    , P.PersistUniqueWrite backend
    , MonadIO m
    )
  => Int
  -> ReaderT backend m ()
myUniqueRecordLooped n = sourceGen updateMyUniqueRecord n $$ mkLoopedSink put

myUniqueRecordBatch
  ::( P.BaseBackend backend ~ P.SqlBackend
    , P.PersistStoreWrite backend
    , P.PersistUniqueWrite backend
    , MonadIO m
    )
  => Int
  -> ReaderT backend m ()
myUniqueRecordBatch n = sourceGen updateMyUniqueRecord n $$ mkBatchSink putMany

updateMyUniqueRecord :: Schema.MyUniqueRecord -> IO Schema.MyUniqueRecord
updateMyUniqueRecord x = do
  time <- Time.getCurrentTime
  let timeStr = Time.formatTime Time.defaultTimeLocale Time.rfc822DateFormat time
  let newCol7 = T.pack timeStr
  return $ x { Schema.myUniqueRecordCol7 = newCol7 }

mkLoopedSink :: Monad m => (a -> m ()) -> C.Sink a m ()
mkLoopedSink a = CL.mapM_ a

mkBatchSink :: Monad m => ([i] -> m ()) -> C.Sink i m ()
mkBatchSink a = CL.chunksOf 10000 .| CL.mapM_ a

e2kr :: P.Entity t -> (P.Key t, t)
e2kr (P.Entity k r) = (k,r)

-- | Insert only PK doesn't already exist.
repsert
  ::( P.PersistRecordBackend record backend
    , P.PersistStoreWrite backend
    , MonadIO m
    )
  => P.Entity record
  -> ReaderT backend m ()
repsert e = uncurry P.repsert . e2kr $ e

-- | Insert many records that don't already exist.
repsertMany
  ::( P.PersistRecordBackend record backend
    , P.PersistStoreWrite backend
    , MonadIO m
    )
  => [P.Entity record]
  -> ReaderT backend m ()
repsertMany es = P.repsertMany . map e2kr $ es

-- | UPSERT a record
put
  ::( P.PersistRecordBackend record backend
    , P.PersistStoreWrite backend
    , P.PersistUniqueRead backend
    , MonadIO m
    )
  => record
  -> ReaderT backend m ()
put r = do
  mr <- if null uks
    then pure Nothing
    else P.getBy $ head uks
  case mr of
    (Just (P.Entity k _)) -> P.replace k r
    Nothing  -> P.insert_ r
  where
    uks = P.persistUniqueKeys $ r

-- | Upsert many records.
putMany
  ::( P.PersistRecordBackend record backend
    , P.PersistUniqueWrite backend
    , MonadIO m
    )
  => [record]
  -> ReaderT backend m ()
putMany rs = P.putMany rs