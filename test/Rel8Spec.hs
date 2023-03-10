{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

import Control.Exception
import Data.Money
import GHC.Generics
import Hasql.Connection (Connection, ConnectionError)
import Hasql.Connection qualified as Hasql
import Hasql.Session qualified as Hasql
import Rel8
import Test.Hspec

instance Exception ConnectionError

data Exc = Exc String
  deriving (Show)

instance Exception Exc

data Bucks f = Bucks { amount :: f (Money USD)
                     } deriving (Generic, Rel8able)

bucksSchema :: TableSchema (Bucks Name)
bucksSchema = TableSchema { schema = Nothing
                          , name = "bucks"
                          , columns = Bucks { amount = "amount" }
                          }

dbsets :: Hasql.Settings
dbsets = Hasql.settings "10.233.1.2" 5432 "bucks" "password" "bucks"

throwLeft :: (Exception e, Applicative f) => Either e a -> f a
throwLeft (Left e) = throw e
throwLeft (Right a) = pure a

onLeft :: Applicative f => Either a b -> (a -> f b) -> f b
onLeft (Left a) f = f a
onLeft (Right b) _ = pure b

createConnection :: IO Connection
createConnection = throwLeft =<< Hasql.acquire dbsets

createTable :: Connection -> IO ()
createTable conn = throwLeft =<< Hasql.run sql conn
  where sql = Hasql.sql "CREATE TABLE bucks (amount money not null);"

dropTable :: Connection -> IO ()
dropTable conn = throwLeft =<< Hasql.run sql conn
  where sql = Hasql.sql "DROP TABLE IF EXISTS bucks;"

truncateTable :: Connection -> IO ()
truncateTable conn = throwLeft =<< Hasql.run sql conn
  where sql = Hasql.sql "TRUNCATE bucks;"

insertMoney :: Connection -> Money USD -> IO ()
insertMoney conn m = throwLeft =<< Hasql.run (Hasql.statement () (insert ins)) conn
  where ins = Insert { into = bucksSchema
                     , rows = values [ Bucks (lit m) ]
                     , onConflict = Abort
                     , returning = pure ()
                     }

fetchMoney :: Connection -> IO (Money USD)
fetchMoney conn = f =<< throwLeft =<< Hasql.run (Hasql.statement () st) conn
  where st = select $ do b <- each bucksSchema
                         return (amount b)
        f [] = throw (Exc "Zero entries returned")
        f [x] = pure x
        f _ = throw (Exc "Multiple entries returned")

main :: IO ()
main = do conn <- createConnection
          dropTable conn
          createTable conn
          hspec $ before_ (truncateTable conn) $ do

            describe "Database tests" $ do
              it "insert == select" $ do
                let m = Money 12.23
                insertMoney conn m
                fetchMoney conn `shouldReturn` m
