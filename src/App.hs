{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module App where

import Lib

import DbBase
import FrontendBase

import Control.Monad.IO.Class
import           Data.Aeson
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO

import Data.ByteString.Lazy.Char8 as C (pack)

-- * api

data HTML

instance Accept HTML where
   contentType _ = "text/html"

instance MimeRender HTML String  where
   mimeRender _ val = C.pack val


type BoardApi =
  "threads" :> Get '[HTML] String :<|>
  "thread" :> Capture "name" String :> Get '[HTML] String

boardApi :: Proxy BoardApi
boardApi = Proxy

-- * app

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
  App.getComments db frontend

getThreads :: DB d => Frontend f => d -> f -> Handler String
getThreads db frontend = liftIO $ do
    threads <- (DbBase.getThreads db)
    return $ allThreadsPage frontend threads

getComments :: DB d => Frontend f => d -> f -> String -> Handler String
getComments db frontend threadName = liftIO $ do
    comments <- DbBase.getThreadComments db (Thread threadName)
    return $ threadPage frontend (Thread threadName) comments

