import DbBase
import FrontendBase
import Lib

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List


data DBMock = DBMock [Thread] [Comment]

instance DB DBMock where
    getThreads (DBMock threads _) = return threads

    getThreadComments (DBMock _ comments) curThread = return $
        filter (\x -> thread x == curThread) comments

    addComment = error "Immutable db"

data FrontendMock = FrontendMock

instance Frontend FrontendMock where
    allThreadsPage _ threads     = intercalate "\n" (map name threads)
    threadPage _ thread comments = intercalate "\n" (map text comments)


main = defaultMain tests


tests = testGroup "Unit tests" 
    [
        testCase "List comparison (different length)" $
            assertEqual "asd should be equal qwe" "asd" "qwe"
    ]
