module Control.Effect.Frontend where

import           Control.Algebra
import           Data.Functor
import           Data.Kind       (Type)
import           Data.Text
import           Lib

data Frontend (m :: Type -> Type) k where
  AllThreadsPage :: [Thread] -> Frontend m Text
  ThreadPage     :: Text -> [Comment] -> Frontend m Text

allThreadsPage :: Has Frontend sig m => [Thread] ->  m Text
allThreadsPage = send . AllThreadsPage
{-# INLINE allThreadsPage #-}

threadPage :: Has Frontend sig m => Text -> [Comment] -> m Text
threadPage = (send .) . ThreadPage
{-# INLINE threadPage #-}
