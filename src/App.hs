module App where

import           Control.Carrier.Lift
import           Control.Carrier.Reader
import           Control.Monad.IO.Class             (liftIO)
import           Data.Aeson
import           Data.ByteString.Lazy.Char8         as C (fromStrict, pack)
import           Data.List
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.Text.Encoding                 (encodeUtf8)
import qualified Data.Text.Lazy                     as L
import           Data.Time.LocalTime
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.API                        (StdMethod (POST))
import           Servant.Server.StaticFiles
import           System.IO
import           Text.HTML.SanitizeXSS
import           Text.Markdown
import           Text.Read
import           Web.FormUrlEncoded                 (FromForm)

import           API
import           Control.Carrier.Frontend.Bootstrap
import           Control.Effect.Frontend
import           Control.Effect.ThreadDB
import           Lib


handleGetThreads :: (Has (Lift IO) sig m, Has ThreadDB sig m, Has Frontend sig m) => m Text
handleGetThreads = do
    threads <- getThreads
    allThreadsPage $ reverse $ sort threads

handleGetComments :: (Has ThreadDB sig m, Has Frontend sig m) => Text -> m Text
handleGetComments threadName = do
    (comments :: [Comment Text]) <- getComments threadName
    let mcomments = (markdown def { msXssProtect = True } . L.fromStrict <$>) <$> comments
    threadPage threadName mcomments

redirect :: Text -> Redirect
redirect a = addHeader a NoContent

threadBan :: String
threadBan = "/\\#?"

handleCreateThread :: (Has ThreadDB sig m, Has Frontend sig m) => CreateThreadForm -> m Redirect
handleCreateThread (CreateThreadForm threadName) = do
  let
    name = T.filter (`notElem` threadBan) $ T.strip $ sanitize threadName
  addThread name
  pure $ redirect ("thread/" <> name)

handleMessage :: (Has (Lift IO) sig m, Has ThreadDB sig m, Has Frontend sig m) => MessageForm -> m Redirect
handleMessage (MessageForm commentText threadName replyToId) = do
    date <- sendIO getZonedTime
    let id_ = readMaybe $ T.unpack replyToId
    addComment $ InsertComment threadName commentText (T.pack $ show date) id_
    pure $ redirect ("thread/" <> threadName)

server
  :: ( Has (Lift IO) sig m
     , Has ThreadDB sig m
     , Has Frontend sig m)
  => Static
  -> ServerT BoardApi m
server static
  =    handleGetThreads
  :<|> handleGetComments
  :<|> handleCreateThread
  :<|> handleMessage
  :<|> serveDirectoryWebApp (T.unpack . unStatic $ static)
