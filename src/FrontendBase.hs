module FrontendBase where

import           Data.Text (Text)

import           Lib

class Frontend a where
    allThreadsPage :: a -> [Thread] -> Text
    threadPage     :: a -> Thread -> [Comment] -> Text

-- class Frontend m where
--   allThreadsPage :: [Thread] -> m Text
--   threadPage     :: Thread -> [Comment] -> m Text
