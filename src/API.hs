module API where

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
import           System.IO
import           Text.Read
import           Web.FormUrlEncoded         (FromForm)

import           Control.Effect.ThreadDB
import           Lib

newtype Static = MkStatic { unStatic :: Text }

data HTML

instance Accept HTML where
   contentType _ = "text/html"

instance MimeRender HTML Text where
   mimeRender _ val = fromStrict $ encodeUtf8 val

type Redirect = (Headers '[Header "Location" Text] NoContent)

newtype CreateThreadForm = CreateThreadForm { threadName :: Text }
  deriving (Eq, Show, Generic)

data MessageForm = MessageForm {
  commentText :: !Text,
  threadName  :: !Text,
  replyToId   :: !Text
  } deriving (Eq, Show, Generic)

instance FromForm CreateThreadForm
instance FromForm MessageForm

type RedirectResponse = Verb 'POST 301 '[HTML] Redirect

type BoardApi
  =     Get '[HTML] Text
  :<|> "thread" :> Capture "name" Text :> Get '[HTML] Text
  :<|> "create_thread" :> ReqBody '[FormUrlEncoded] CreateThreadForm :> RedirectResponse
  :<|> "message" :> ReqBody '[FormUrlEncoded] MessageForm :> RedirectResponse
  :<|> "static" :> Raw

boardApi :: Proxy BoardApi
boardApi = Proxy
