{-# LANGUAGE DeriveGeneric #-}

module Lib where

import           GHC.Generics
import           Data.Aeson
import Data.Text


data Thread = Thread {
    name :: Text
} deriving (Eq, Show, Generic)


data Comment = Comment {
    id_ :: Int,
    threadName :: Text,
    text :: Text,
    date :: String,
    replyToId :: Maybe Int
} deriving (Eq, Show, Generic)


instance ToJSON Thread
instance FromJSON Thread

instance ToJSON Comment
instance FromJSON Comment
