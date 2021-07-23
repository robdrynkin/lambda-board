module FrontendBase where

import           Data.Text (Text)

import           Lib

class Frontend a where
    allThreadsPage :: a -> [Thread] -> Text
    threadPage     :: a -> Text -> [Comment] -> Text
