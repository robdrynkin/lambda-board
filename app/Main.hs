module Main where

import           App

import           BootstrapFrontend
import qualified Data.Text.IO           as T
import           Database.SQLite.Simple
import           Lib
import           SqliteDb


db :: LiteDb
db = LiteDb $ open "test.db"

frontend :: IO BootstrapFrontend
frontend = do
    threadsTemplate <- T.readFile "src/bootstrap_static/threads.html"
    commentsTemplate <- T.readFile "src/bootstrap_static/comments.html"
    return $ BootstrapFrontend threadsTemplate commentsTemplate


loadApp :: IO()
loadApp = do
    putStrLn "Starting..."

main :: IO ()
-- main = loadApp >> run db frontend
main = do
    loadApp
    f <- frontend
    run db f
