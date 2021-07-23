{-# LANGUAGE DefaultSignatures #-}

module DbBase where

import           Data.Functor
import           Data.List    (sort)
import           Data.Text
import           Lib

class Monad m => HasDB m where
    getThreads :: m [Thread]
    default getThreads :: m [Thread]
    getThreads = sort <$> getThreadsInner

    getThreadsInner   :: m [Thread]
    getThreadComments :: Text -> m [Comment]
    addThread         :: Text -> m ()
    addComment        :: Comment -> m ()
