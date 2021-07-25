module Control.Carrier.ThreadDB.Sqlite where

import           Control.Algebra
import           Control.Carrier.Lift
import           Control.Carrier.Reader
import           Data.Text                      (Text)
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow

import           Control.Effect.ThreadDB
import           Lib

newtype LiteDb = MkLiteDb { unLiteDb :: Connection }

instance FromRow Thread where
    fromRow = Thread <$> field <*> field
instance ToRow Thread where
    toRow (Thread name ncomments) = toRow (name, ncomments)

instance FromRow (Comment Text) where
  fromRow = Comment <$> field <*> field <*> field <*> field <*> field
instance ToRow InsertComment where
    toRow (InsertComment threadName text date replyToId) = toRow (threadName, text, date, replyToId)

newtype SqliteC m a = MkSqliteC { runSqlite :: m a }
  deriving (Functor, Applicative, Monad)

instance ( Has (Lift IO) sig m, Has (Reader LiteDb) sig m ) => Algebra (ThreadDB :+: sig) (SqliteC m) where
  alg hdl sig ctx = case sig of
    R other -> MkSqliteC $ alg (runSqlite . hdl) other ctx

    L GetThreads -> do
      MkLiteDb conn <- ask
      (<$ctx) <$> sendIO (query_ conn "select * from threads")

    L (GetComments threadName) -> do
      MkLiteDb conn <- ask
      r <- sendIO $ query @(Only Text) @(Only Int) conn "select 1 from threads where name == (?)" (Only threadName)
      case r of
        [] -> pure $ [] <$ ctx -- FIXME throw 404
        _  -> sendIO $ (<$ ctx) <$> query conn "select * from comments where threadName == (?)" (Only threadName)

    L (AddComment comment) -> (<$ ctx) <$> do
      MkLiteDb conn <- ask
      sendIO do
        execute conn "INSERT INTO comments (threadName, text, date, replyToId) VALUES (?,?,?,?)" comment
        execute conn "UPDATE threads SET ncomments = ncomments + 1 WHERE name == (?)" (Only (ithreadName comment))

    L (AddThread threadName) -> (<$ctx) <$> do
      MkLiteDb conn <- ask
      sendIO $ execute conn "INSERT INTO threads (name, ncomments) VALUES (?,?)" (Thread threadName 0)
