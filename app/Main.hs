module Main where

import           App

import           BootstrapFrontend
import           Data.Semigroup         ((<>))
import qualified Data.Text.IO           as T
import           Database.SQLite.Simple
import           Lib
import           Options.Applicative
import           SqliteDb
import           Control.Monad.Reader


data CLI = CLI {
  db   :: String,
  port :: Int
}


sample :: Parser CLI
sample = CLI
      <$> strOption
          ( long "db"
         <> short 'd'
         <> metavar "FILE"
         <> help "File for sqlite db" )
      <*> option auto
          ( long "port"
         <> short 'p'
         <> help "Port for board to listen to"
         <> metavar "INT" )


getDb :: String -> IO LiteDb
getDb f = MkLiteDb <$> open f

frontend :: IO BootstrapFrontend
frontend = do
    threadsTemplate <- T.readFile "src/bootstrap_static/threads.html"
    commentsTemplate <- T.readFile "src/bootstrap_static/comments.html"
    return $ BootstrapFrontend threadsTemplate commentsTemplate


parseArgs :: IO CLI
parseArgs = do
  let opts = info (sample <**> helper)
        ( fullDesc
        <> progDesc "Lalambda board app"
        <> header "Great thing. Never use in production." )
  execParser opts

main :: IO ()
main = do
    CLI dbName port <- parseArgs
    f <- frontend
    putStrLn "Starting..."
    dbc <- getDb dbName
    _ <- runReaderT (run port f) dbc
    pure ()
