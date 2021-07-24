{-# LANGUAGE DuplicateRecordFields #-}

module App where

import           Lib

import           DbBase
import           FrontendBase

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Aeson
import           Data.ByteString.Lazy.Char8 as C (fromStrict, pack)
import           Data.Text
import           Data.Text.Encoding         (encodeUtf8)
import           Data.Time.LocalTime
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.API                (StdMethod (POST))
import           Servant.Server.StaticFiles
import           SqliteDb
import           System.IO
import           Text.HTML.SanitizeXSS
import           Text.Read
import           Web.FormUrlEncoded         (FromForm)


type AppM = ReaderT LiteDb Handler

-- * api

data HTML

instance Accept HTML where
   contentType _ = "text/html"

instance MimeRender HTML Text where
   mimeRender _ val = fromStrict $ encodeUtf8 val

type Redirect = (Headers '[Header "Location" Text] NoContent)

data CreateThreadForm = CreateThreadForm { threadName :: Text } deriving (Eq, Show, Generic)
data MessageForm = MessageForm {
    commentText :: Text,
    threadName  :: Text,
    replyToId   :: Text
} deriving (Eq, Show, Generic)

instance FromForm CreateThreadForm
instance FromForm MessageForm

type BoardApi = Get '[HTML] Text
   :<|> "thread" :> Capture "name" Text :> Get '[HTML] Text
   :<|> "create_thread" :> ReqBody '[FormUrlEncoded] CreateThreadForm :> Verb 'POST 301 '[HTML] Redirect
   :<|> "message" :> ReqBody '[FormUrlEncoded] MessageForm :> Verb 'POST 301 '[HTML] Redirect
   :<|> "static" :> Raw


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
server frontend = App.getThreads frontend
  :<|> App.getComments frontend
  :<|> App.createThread frontend
  :<|> App.message frontend
  :<|> serveDirectoryWebApp "static"


getThreads :: Frontend f => f -> AppM Text
getThreads frontend = allThreadsPage frontend <$> DbBase.getThreads


getComments :: Frontend f => f -> Text -> AppM Text
getComments frontend threadName = do
    comments <- DbBase.getThreadComments threadName
    pure $ threadPage frontend threadName comments

redirect :: Text -> Redirect
redirect a = addHeader a NoContent

createThread :: Frontend f => f -> CreateThreadForm -> AppM Redirect
createThread frontend (CreateThreadForm threadName) = do
    DbBase.addThread threadName
    pure $ redirect ("thread/" <> sanitize threadName)

message :: Frontend f => f -> MessageForm -> AppM Redirect
message frontend (MessageForm commentText threadName replyToId) = do
    date <- liftIO getZonedTime
    let (id_ :: Maybe Int) = readMaybe (unpack replyToId)
    let sanitizedComment = sanitize commentText
    DbBase.addComment (InsertComment threadName sanitizedComment (Data.Text.pack (show date)) id_)
    pure $ redirect ("thread/" <> threadName)
