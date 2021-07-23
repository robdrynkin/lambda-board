module PostgresDb where

import           DbBase
import           Lib

-- import Database.PostgreSQL.Simple


-- data PgDb = PgDb [Thread] [Comment]
-- 
-- instance DB PgDb where
--     getThreads (PgDb threads _) = pure threads
-- 
--     getThreadComments (PgDb _ comments) curThread = pure $
--         filter (\x -> threadName x == name curThread) comments
-- 
--     addComment = error "Immutable db"
