{-# LANGUAGE DefaultSignatures #-}

module DbBase where

import           Data.Functor
import           Data.List    (sort)
import           Lib

class Monad m => HasDB m where
  getThreads :: m [Thread]
  default getThreads :: m [Thread]
  getThreads = sort <$> getThreadsInner

  getThreadsInner   :: m [Thread]
  getThreadComments :: Thread -> m [Comment]
  addThread         :: Thread -> m ()
  addComment        :: Comment -> m ()
