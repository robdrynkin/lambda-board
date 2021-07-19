module PostgresDb where

import DbBase
import Lib

data PgDb = PgDb [Thread] [Comment]

instance DB PgDb where
    getThreads (PgDb threads _) = return threads

    getThreadComments (PgDb _ comments) curThread = return $
        filter (\x -> name (thread x) == name curThread) comments

    addComment = error "Immutable db"
