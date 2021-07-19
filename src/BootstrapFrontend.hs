module BootstrapFrontend where

import           FrontendBase
import           Lib

import           Data.List


data BootstrapFrontend = BootstrapFrontend

instance Frontend BootstrapFrontend where
    allThreadsPage _ threads     = intercalate " <br> " (map name threads)
    threadPage _ thread comments = intercalate " <br> " (map text comments)

