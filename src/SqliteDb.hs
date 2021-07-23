{-# LANGUAGE OverloadedStrings #-}

module SqliteDb where

import           DbBase
import           Lib

import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow


newtype LiteDb = LiteDb (IO Connection)

instance FromRow Thread where
    fromRow = Thread <$> field <*> field
instance ToRow Thread where
    toRow (Thread name ncomments) = toRow (name, ncomments)

instance FromRow Comment where
  fromRow = Comment <$> field <*> field <*> field <*> field <*> field
instance ToRow Comment where
    toRow (Comment id_ threadName text date replyToId) = toRow (id_, threadName, text, date, replyToId)


instance DB LiteDb where
    getThreadsInner (LiteDb conn) = conn >>= flip query_ "select * from threads"

    getThreadComments (LiteDb conn) curThread = conn >>= \c ->
        query c "select * from comments where threadName == (?)" (Only (name curThread))

    addComment (LiteDb conn) comment = conn >>= \c -> do
        execute c "INSERT INTO comments (id, threadName, text, date, replyToId) VALUES (?,?,?,?,?);" comment
        execute c "UPDATE threads SET ncomments = ncomments + 1 WHERE threadName = (?)" (Only (threadName comment))
        return ()

    addThread (LiteDb conn) thread = conn >>= \c ->
        execute c "INSERT INTO threads (name) VALUES (?)" thread
