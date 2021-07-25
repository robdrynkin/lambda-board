{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Carrier.Lift
import           Control.Carrier.Reader
import           Control.Monad.IO.Class
import           Data.FileEmbed
import           Data.Maybe
import           Data.Semigroup                     ((<>))
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.Text.Encoding     (decodeUtf8)
import qualified Data.Text.IO                       as T
import           Database.SQLite.Simple
import           Options.Applicative
import           Network.Wai
import           Network.Wai.Handler.Warp
import           System.IO
import           Servant

import           API
import           App
import           Control.Carrier.Frontend.Bootstrap
import           Control.Carrier.ThreadDB.Sqlite
import           Lib

data CLI = MkCLI
  { db     :: !Text
  , port   :: !Int
  , static :: !Static }

sample :: Parser CLI
sample = MkCLI
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
      <*> (MkStatic <$> strOption
          ( long "static"
         <> short 's'
         <> help "Directory with static files"
         <> metavar "DIR" ))

getDb :: Text -> IO LiteDb
getDb f = MkLiteDb <$> open (T.unpack f)

frontend :: IO BootstrapFrontend
frontend = do
  let threadsTemplate = decodeUtf8 $ fromJust $(embedFileIfExists "src/bootstrap_static/threads.html")
  let commentsTemplate = decodeUtf8 $ fromJust $(embedFileIfExists "src/bootstrap_static/comments.html")
  pure $ BootstrapFrontend threadsTemplate commentsTemplate

parseArgs :: IO CLI
parseArgs = do
  let opts = info (sample <**> helper)
        ( fullDesc
        <> progDesc "Lalambda board app"
        <> header "Great thing. Never use in production." )
  execParser opts

main :: IO ()
main = do
  MkCLI dbName port static <- parseArgs
  putStrLn "Starting..."
  db <- getDb dbName
  fi <- frontend
  _ <- runM @IO $ runReader static $ runReader db $ runReader fi $ runApp port
  pure ()

type SNT a
  =  SqliteC (BootstrapC (ReaderC BootstrapFrontend (ReaderC LiteDb (LiftC IO)))) a
  -> Servant.Handler a

phi :: LiteDb -> BootstrapFrontend -> SNT a
phi db fi hh = liftIO $ runM $ runReader db $ runReader fi $ runBootstrap $ runSqlite hh

runApp
  :: ( Has (Lift IO) sig m
     , Has (Reader Static) sig m
     , Has (Reader LiteDb) sig m
     , Has (Reader BootstrapFrontend) sig m )
  => Int
  -> m ()
runApp port = do
  let
    settings =
      setPort port $
      setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port))
      defaultSettings
  app <- mkApp
  sendIO $ runSettings settings app

mkApp
  :: ( Has (Lift IO) sig m
     , Has (Reader Static) sig m
     , Has (Reader LiteDb) sig m
     , Has (Reader BootstrapFrontend) sig m )
  => m Application
mkApp = do
  static <- ask
  db <- ask
  fi <- ask
  pure $ serve boardApi $ hoistServer boardApi (phi db fi) $ server static
