{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module App where

import           Lib

import           DbBase
import           FrontendBase

import           Control.Monad.IO.Class
import           Data.Aeson
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO
import Data.Text
import Data.Text.Encoding ( encodeUtf8 )
import Data.ByteString.Lazy.Char8 as C (pack, fromStrict)

-- * api

data HTML

instance Accept HTML where
   contentType _ = "text/html"

instance MimeRender HTML Text where
   mimeRender _ val = fromStrict $ encodeUtf8 val


type BoardApi =
  "threads" :> Get '[HTML] Text :<|>
  "thread" :> Capture "name" Text :> Get '[HTML] Text :<|>
  "createThread" :> Post '[HTML] Text

boardApi :: Proxy BoardApi
boardApi = Proxy


run :: DB d => Frontend f => d -> f -> IO ()
run db frontend = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< mkApp db frontend

mkApp :: DB d => Frontend f => d -> f -> IO Application
mkApp db frontend = return $ serve boardApi $ server db frontend

server :: DB d => Frontend f => d -> f -> Server BoardApi
server db frontend =
  App.getThreads db frontend :<|>
  App.getComments db frontend :<|>
  App.createThread db frontend




getThreads :: DB d => Frontend f => d -> f -> Handler Text
getThreads db frontend = liftIO $ do
    threads <- (DbBase.getThreads db)
    return $ allThreadsPage frontend threads


getComments :: DB d => Frontend f => d -> f -> Text -> Handler Text
getComments db frontend threadName = liftIO $ do
    comments <- DbBase.getThreadComments db (Thread threadName)
    return $ threadPage frontend (Thread threadName) comments


createThread :: DB d => Frontend f => d -> f -> Handler Text
createThread db frontend = return ""
