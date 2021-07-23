module SqliteDb where

import           DbBase
import           Lib

import           Control.Monad.Reader
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow


newtype LiteDb = MkLiteDb { unLiteDb :: IO Connection }

instance FromRow Thread where
    fromRow = Thread <$> field <*> field
instance ToRow Thread where
    toRow (Thread name ncomments) = toRow (name, ncomments)

instance FromRow Comment where
  fromRow = Comment <$> field <*> field <*> field <*> field <*> field
instance ToRow Comment where
    toRow (Comment id_ threadName text date replyToId) = toRow (id_, threadName, text, date, replyToId)


instance (Monad m, MonadIO m, MonadReader LiteDb m) => HasDB m where
    getThreadsInner = do
        conn <- asks unLiteDb >>= liftIO
        liftIO $ query_ conn "select * from threads"

    getThreadComments threadName = do
        conn <- asks unLiteDb >>= liftIO
        liftIO $ query conn "select * from comments where threadName == (?)" (Only threadName)

    addComment comment = do
      conn <- asks unLiteDb >>= liftIO
      liftIO do
        execute conn "INSERT INTO comments (id, threadName, text, date, replyToId) VALUES (?,?,?,?,?)" comment
        execute conn "UPDATE threads SET ncomments = ncomments + 1 WHERE threadName = (?)" (Only (threadName comment))

    addThread threadName = do
        conn <- asks unLiteDb >>= liftIO
        liftIO $ execute conn "INSERT INTO threads (name, ncomments) VALUES (?,?)" (Thread threadName 0)
