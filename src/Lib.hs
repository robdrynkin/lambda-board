{-# LANGUAGE DeriveGeneric #-}

module Lib where

import           Data.Aeson
import           GHC.Generics


data Thread = Thread {
    name :: String
} deriving (Eq, Show, Generic)


data Comment = Comment {
    id_        :: Int,
    threadName :: String,
    text       :: String,
    date       :: String,
    replyToId  :: Maybe Int
} deriving (Eq, Show, Generic)


instance ToJSON Thread
instance FromJSON Thread

instance ToJSON Comment
instance FromJSON Comment
