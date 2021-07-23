module App where

import           Lib

import           DbBase
import           FrontendBase

import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.ByteString.Lazy.Char8 as C (fromStrict, pack)
import           Data.Text
import           Data.Text.Encoding         (encodeUtf8)
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO
import           Control.Monad.Reader
import           SqliteDb

type AppM = ReaderT LiteDb Handler

-- * api

data HTML

instance Accept HTML where
   contentType _ = "text/html"

instance MimeRender HTML Text where
   mimeRender _ val = fromStrict $ encodeUtf8 val


type BoardApi
  =    "threads" :> Get '[HTML] Text
  :<|> "thread" :> Capture "name" Text :> Get '[HTML] Text
  :<|> "createThread" :> Post '[HTML] Text

boardApi :: Proxy BoardApi
boardApi = Proxy


run :: (MonadIO m, MonadReader LiteDb m, Frontend f) => Int -> f -> m ()
run port frontend = do
  let settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port))
        defaultSettings
  app <- mkApp frontend
  liftIO $ runSettings settings app

mkApp :: (MonadIO m, MonadReader LiteDb m, Frontend f) => f -> m Application
mkApp frontend = do
  db <- ask @LiteDb
  pure $ serveWithContext boardApi EmptyContext $
    hoistServerWithContext boardApi (Proxy :: Proxy '[]) (`runReaderT` db) $ server frontend

server :: Frontend f => f -> ServerT BoardApi AppM
server frontend =
  App.getThreads frontend :<|>
  App.getComments frontend :<|>
  App.createThread frontend


getThreads :: Frontend f => f -> AppM Text
getThreads frontend = do
    threads <- DbBase.getThreads
    pure $ allThreadsPage frontend threads


getComments :: Frontend f => f -> Text -> AppM Text
getComments frontend threadName = do
    let thread = Thread threadName 0
    comments <- DbBase.getThreadComments thread
    pure $ threadPage frontend thread comments


createThread :: (Frontend f) => f -> AppM Text
createThread frontend = DbBase.addThread (Thread "asdf" 0) >> pure ""
