import           DbBase
import           FrontendBase
import           Lib

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC
import           Test.Tasty.SmallCheck as SC

import           Data.List


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


b  = Thread "/b"
bb = Thread "/bb"

db :: DBMock
db = DBMock
    [b, bb]
    [
        Comment {threadName = name b, text = "Some b text 1"},
        Comment {threadName = name b, text = "Some b text 2"},
        Comment {threadName = name bb, text = "Some bb text 1"},
        Comment {threadName = name bb, text = "Some bb text 2"},
        Comment {threadName = name bb, text = "Some bb text 3"}
    ]


main = defaultMain tests


tests = testGroup "Unit tests"
    [
        testCase "List comparison (different length)" $
            assertEqual "asd should be equal qwe" "asd" "qwe"
    ]
