{-# LANGUAGE OverloadedStrings          #-}

module Main where

import           Database.PersistExample.Stream

import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Logger    (runNoLoggingT)
import           Database.Persist.MySQL

connectionInfo :: MySQLConnectInfo
connectionInfo = mkMySQLConnectInfo "localhost" "test" "test" "example"

main :: IO ()
main = runNoLoggingT $ withMySQLPool connectionInfo 10 $ \pool -> liftIO $ do
          flip runSqlPersistMPool pool $ do
            -- createDB
            doList
