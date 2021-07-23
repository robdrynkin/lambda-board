{-# LANGUAGE DefaultSignatures #-}

module DbBase where

import           Data.Functor
import           Data.List    (sort)
import           Lib

class DB a where
    getThreads :: a -> IO [Thread]
    default getThreads :: a -> IO [Thread]
    getThreads a = (getThreadsInner a) <&> (\a -> sort a)

    getThreadsInner   :: a -> IO [Thread]
    getThreadComments :: a -> Thread -> IO [Comment]
    addThread         :: a -> Thread -> IO ()
    addComment        :: a -> Comment -> IO ()
