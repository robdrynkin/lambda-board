module DbBase where

import           Lib

class DB a where
    getThreads        :: a -> IO [Thread]
    getThreadComments :: a -> Thread -> IO [Comment]
    addThread         :: a -> Thread -> IO ()
    addComment        :: a -> Comment -> IO ()
