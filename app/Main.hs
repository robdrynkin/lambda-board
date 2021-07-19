module Main where

import App

import Lib
import SqliteDb
import Database.SQLite.Simple
import BootstrapFrontend

db :: LiteDb
db = LiteDb $ open "test.db"

frontend :: BootstrapFrontend
frontend = BootstrapFrontend 

loadApp :: IO()
loadApp = do
    putStrLn "Starting..."

main :: IO ()
main = loadApp >> run db frontend
