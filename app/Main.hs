{-# LANGUAGE TemplateHaskell #-}

module Main where

import           App

import           BootstrapFrontend
import           Control.Monad.Reader
import           Data.FileEmbed
import           Data.Maybe
import           Data.Semigroup         ((<>))
import           Data.Text
import           Data.Text.Encoding     (decodeUtf8)
import qualified Data.Text.IO           as T
import           Database.SQLite.Simple
import           Lib
import           Options.Applicative
import           SqliteDb


data CLI = CLI { db     :: Text
               , port   :: Int
               , static :: Text
               }


sample :: Parser CLI
sample = CLI
      <$> strOption
          ( long "db"
         <> short 'd'
         <> help "File for sqlite db"
         <> metavar "FILE" )
      <*> option auto
          ( long "port"
         <> short 'p'
         <> help "Port for board to listen to"
         <> metavar "INT" )
      <*> strOption
          ( long "static"
         <> short 's'
         <> help "Directory with static files"
         <> metavar "DIR" )


getDb :: Text -> IO LiteDb
getDb f = MkLiteDb <$> open (Data.Text.unpack f)

frontend :: IO BootstrapFrontend
frontend = do
    let threadsTemplate = decodeUtf8 $ fromJust $(embedFileIfExists "src/bootstrap_static/threads.html")
    let commentsTemplate = decodeUtf8 $ fromJust $(embedFileIfExists "src/bootstrap_static/comments.html")
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
    CLI dbName port static <- parseArgs
    f <- frontend
    putStrLn "Starting..."
    dbc <- getDb dbName
    _ <- runReaderT (run static port f) dbc
    pure ()
