module Lib where


data Thread = Thread {
    name :: String
} deriving (Eq, Show)


data Comment = Comment {
    id_ :: Int,
    thread :: Thread,
    text :: String,
    date :: String,
    replyToId :: Int
} deriving (Eq, Show)
