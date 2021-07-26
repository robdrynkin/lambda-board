module App where

import           Control.Carrier.Lift
import           Control.Carrier.Reader
import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Random
import           Data.Aeson
import           Data.ByteString.Lazy.Char8         as C (fromStrict, pack)
import           Data.List
import           Data.String                        (IsString (..))
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.Text.Encoding                 (encodeUtf8)
import qualified Data.Text.Lazy                     as L
import           Data.Time.LocalTime
import           Debug.Trace
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.API                        (StdMethod (POST))
import           Servant.Server.StaticFiles
import           System.Directory
import           System.IO
import           Text.Blaze.Html5
import qualified Text.Blaze.Html5.Attributes        as A
import           Text.HTML.SanitizeXSS
import           Text.Markdown
import           Text.Read
import           Web.FormUrlEncoded                 (FromForm)

import           API
import           Control.Carrier.Frontend.Bootstrap
import           Control.Effect.Frontend
import           Control.Effect.ThreadDB
import           Lib


randomPick :: (MonadRandom m) => [a] -> m a
randomPick arr = do
    rand <- getRandomR (0, length arr - 1)
    return $ arr !! rand


coqHandler :: FilePath -> (Text, Html) -> Html
coqHandler path (t, _) =
    let
      cucked_path = "/static/coq/" <> path
      style img = trace img $ "background-image: url('" <> img <> "'); background-size: 100% 100%;"
    in
      img ! A.src (fromString cucked_path) ! A.width "30%"

htmlBlockHandlers :: FilePath -> Maybe Text -> (Text, Html) -> Html
htmlBlockHandlers path mLang = case T.toLower <$> mLang of
  Just "coq" -> coqHandler path
  _          -> msBlockCodeRenderer defaultMarkdownSettings mLang

handleGetThreads :: (Has (Lift IO) sig m, Has ThreadDB sig m, Has Frontend sig m) => m Text
handleGetThreads = do
    threads <- getThreads
    allThreadsPage $ reverse $ sort threads

handleGetComments :: (Has ThreadDB sig m, Has (Lift IO) sig m, Has Frontend sig m, MonadRandom m) => Text -> Text -> m Text
handleGetComments static threadName = do
    comments <- getComments threadName
    files <- sendIO $ listDirectory $ T.unpack $ static <> "/coq"
    file <- randomPick files
    let block = htmlBlockHandlers file
    let mdown = markdown def { msXssProtect = True, msBlockCodeRenderer = block }
    let mcomments = (mdown . L.fromStrict <$>) <$> comments
    threadPage threadName mcomments

redirect :: Text -> Redirect
redirect a = addHeader a NoContent

threadBan :: String
threadBan = "/\\#?"

handleCreateThread :: (Has ThreadDB sig m, Has Frontend sig m) => CreateThreadForm -> m Redirect
handleCreateThread (CreateThreadForm threadName token) = do
  let
    name = T.filter (`notElem` threadBan) $ T.strip $ sanitize threadName
  addThread (name, token)
  pure $ redirect ("thread/" <> name)

handleMessage :: (Has (Lift IO) sig m, Has ThreadDB sig m, Has Frontend sig m) => MessageForm -> m Redirect
handleMessage (MessageForm commentText threadName replyToId) = do
    date <- sendIO getZonedTime
    let id_ = readMaybe $ T.unpack replyToId
    addComment $ InsertComment threadName commentText (T.pack $ show date) id_
    pure $ redirect ("thread/" <> threadName)

handleDeleteComment :: (Has (Lift IO) sig m, Has ThreadDB sig m, Has Frontend sig m) => DeleteCommentForm -> m Redirect
handleDeleteComment (DeleteCommentForm commentId threadName token) = do
    deleteComment (DeleteComment commentId threadName token)
    pure $ redirect ("thread/" <> threadName)

server
  :: ( Has (Lift IO) sig m
     , Has ThreadDB sig m
     , Has Frontend sig m
     , MonadRandom m )
  => Static
  -> ServerT BoardApi m
server static
  =    handleGetThreads
  :<|> handleGetComments (T.pack . T.unpack . unStatic $ static)
  :<|> handleCreateThread
  :<|> handleMessage
  :<|> handleDeleteComment
  :<|> serveDirectoryWebApp (T.unpack . unStatic $ static)
