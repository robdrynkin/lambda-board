module Control.Effect.ThreadDB where

import           Control.Algebra
import           Data.Functor
import           Data.Kind       (Type)
import           Data.Text
import           Lib

data ThreadDB (m :: Type -> Type) k where
  GetThreads  :: ThreadDB m [Thread]
  GetComments :: Text -> ThreadDB m [Comment Text]
  AddThread   :: Text -> ThreadDB m ()
  AddComment  :: InsertComment -> ThreadDB m ()

getThreads :: Has ThreadDB sig m => m [Thread]
getThreads = send GetThreads
{-# INLINE getThreads #-}

getComments :: Has ThreadDB sig m => Text -> m [Comment Text]
getComments = send . GetComments
{-# INLINE getComments #-}

addThread :: Has ThreadDB sig m => Text -> m ()
addThread = send . AddThread
{-# INLINE addThread #-}

addComment  :: Has ThreadDB sig m => InsertComment -> m ()
addComment = send . AddComment
{-# INLINE addComment #-}
