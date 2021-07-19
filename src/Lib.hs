{-# LANGUAGE DeriveGeneric #-}

module Lib where

import           GHC.Generics
import           Data.Aeson


data Thread = Thread {
    name :: String
} deriving (Eq, Show, Generic)


data Comment = Comment {
    id_ :: Int,
    thread :: Thread,
    text :: String,
    date :: String,
    replyToId :: Int
} deriving (Eq, Show, Generic)


instance ToJSON Thread
instance FromJSON Thread

instance ToJSON Comment
instance FromJSON Comment
