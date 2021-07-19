module Main where

import           App

import           BootstrapFrontend
import           Database.SQLite.Simple
import           Lib
import           SqliteDb

db :: LiteDb
db = LiteDb $ open "test.db"

frontend :: BootstrapFrontend
frontend = BootstrapFrontend

loadApp :: IO()
loadApp = do
    putStrLn "Starting..."

main :: IO ()
main = loadApp >> run db frontend
