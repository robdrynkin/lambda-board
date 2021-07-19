module FrontendBase where

import           Lib

class Frontend a where
    allThreadsPage :: a -> [Thread] -> String
    threadPage     :: a -> Thread -> [Comment] -> String
