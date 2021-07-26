module Control.Carrier.ThreadDB.MigrateSqLiteToPostgres where

import           Control.Carrier.ThreadDB.Postgres
import           Control.Carrier.ThreadDB.Sqlite
import           Data.ByteString
import           Data.Text
import           Database.PostgreSQL.Simple        as Postgres
import           Database.PostgreSQL.Simple.ToRow  as PgToRow
import           Database.SQLite.Simple            as Sqlite
import           Lib

data IntermediateDb = IntermediateDb [Thread] [Comment Text]


instance PgToRow.ToRow (Comment Text) where
    toRow (Comment id_ threadName text date replyToId) = PgToRow.toRow (id_, threadName, text, date, replyToId)


readFromSqlite :: String -> IO IntermediateDb
readFromSqlite dbPath = do
    conn <- open dbPath
    threads <- (Sqlite.query_ conn "select * from threads" :: IO [Thread])
    IntermediateDb threads <$> (Sqlite.query_ conn "select * from comments" :: IO [Comment Text])


writeToPostgres :: ByteString -> IntermediateDb -> IO ()
writeToPostgres connUri (IntermediateDb threads comments) = do
    conn <- Postgres.connectPostgreSQL connUri
    _ <- Postgres.executeMany conn "INSERT INTO threads (name, ncomments) VALUES (?,?)" threads
    _ <- Postgres.executeMany conn "INSERT INTO comments (id, threadName, text, date, replyToId) VALUES (?,?,?,?,?)" comments
    return ()


createPgDb :: ByteString -> IO()
createPgDb connUri = do
    conn <- Postgres.connectPostgreSQL connUri
    _ <- Postgres.execute_ conn "CREATE TABLE threads (name TEXT NOT NULL PRIMARY KEY, ncomments INTEGER)"
    _ <- Postgres.execute_ conn "CREATE TABLE comments (id SERIAL, threadName TEXT, text TEXT, date TEXT, replyToId INTEGER);"
    _ <- Postgres.execute_ conn "CREATE INDEX comments_thread_name_index ON comments USING hash (threadName)"
    return ()


migrate :: IO()
migrate = let sqlitePath = "test.db" ; pgConnPath = "postgres://postgres@postgres/database" in
    createPgDb pgConnPath >> readFromSqlite sqlitePath >>= writeToPostgres pgConnPath
