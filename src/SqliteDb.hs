{-# LANGUAGE OverloadedStrings #-}

module SqliteDb where

import           DbBase
import           Lib

import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow


newtype LiteDb = LiteDb (IO Connection)

instance FromRow Thread where
  fromRow = Thread <$> field
instance ToRow Thread where
    toRow (Thread name) = toRow [name]

instance FromRow Comment where
  fromRow = Comment <$> field <*> field <*> field <*> field <*> field
instance ToRow Comment where
    toRow (Comment id_ threadName text date replyToId) = toRow (id_, threadName, text, date, replyToId)


instance DB LiteDb where
    getThreads (LiteDb conn) = conn >>= flip query_ "select * from threads"

    getThreadComments (LiteDb conn) curThread = conn >>= \c ->
        query c "select * from comments where threadName == (?)" (Only (name curThread))

    addComment (LiteDb conn) comment = conn >>= \c ->
        execute c "INSERT INTO comments (id, threadName, text, date, replyToId) VALUES (?,?,?,?,?)" comment
