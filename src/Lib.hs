{-# LANGUAGE DeriveGeneric #-}

module Lib where

import           Data.Aeson
import           Data.Text
import           GHC.Generics


data Thread = Thread {
    name      :: !Text,
    ncomments :: !Int
} deriving (Eq, Show, Generic)

instance Ord Thread where
    (<=) a b = ncomments a <= ncomments b


data Comment = Comment {
    id_        :: !Int,
    threadName :: !Text,
    text       :: !Text,
    date       :: !Text,
    replyToId  :: !(Maybe Int)
} deriving (Eq, Show, Generic)

data InsertComment = InsertComment {
    ithreadName :: !Text,
    itext       :: !Text,
    idate       :: !Text,
    ireplyToId  :: !(Maybe Int)
} deriving (Eq, Show, Generic)


instance ToJSON Thread
instance FromJSON Thread

instance ToJSON Comment
instance FromJSON Comment
