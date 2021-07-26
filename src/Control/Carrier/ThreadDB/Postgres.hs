module Control.Carrier.ThreadDB.Postgres where

import           Control.Algebra
import           Control.Carrier.Lift
import           Control.Carrier.Reader
import           Data.Text                          (Text)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToRow

import           Control.Effect.ThreadDB
import           Lib

newtype PgDb = MkPgDb { conn :: Connection }

instance FromRow Thread where
    fromRow = Thread <$> field <*> field
instance ToRow Thread where
    toRow (Thread name ncomments) = toRow (name, ncomments)

instance FromRow (Comment Text) where
    fromRow = Comment <$> field <*> field <*> field <*> field <*> field
instance ToRow InsertComment where
    toRow (InsertComment threadName text date replyToId) = toRow (threadName, text, date, replyToId)

newtype PgC m a = MkPgC { runPg :: m a }
  deriving (Functor, Applicative, Monad)

instance ( Has (Lift IO) sig m, Has (Reader PgDb) sig m ) => Algebra (ThreadDB :+: sig) (PgC m) where
  alg hdl sig ctx = case sig of
    R other -> MkPgC $ alg (runPg . hdl) other ctx

    L GetThreads -> do
      MkPgDb conn <- ask
      (<$ctx) <$> sendIO (query_ conn "select * from threads")

    L (GetComments threadName) -> do
      MkPgDb conn <- ask
      r <- sendIO $ query @(Only Text) @(Only Int) conn "select 1 from threads where name = (?)" (Only threadName)
      case r of
        [] -> pure $ [] <$ ctx -- FIXME throw 404
        _  -> sendIO $ (<$ ctx) <$> query conn "select * from comments where threadName = (?)" (Only threadName)

    L (AddComment comment) -> (<$ ctx) <$> do
      MkPgDb conn <- ask
      sendIO do
        execute conn "INSERT INTO comments (threadName, text, date, replyToId) VALUES (?,?,?,?)" comment
        execute conn "UPDATE threads SET ncomments = ncomments + 1 WHERE name = (?)" (Only (ithreadName comment))
        return ()

    L (AddThread threadName) -> (<$ctx) <$> do
      MkPgDb conn <- ask
      sendIO $ execute conn "INSERT INTO threads (name, ncomments) VALUES (?,?)" (Thread threadName 0)
      return ()
