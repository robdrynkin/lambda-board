module Main where

import App

import Lib
import PostgresDb
import BootstrapFrontend

b  = Thread "/b"
bb = Thread "/bb"
db :: PgDb 
db = PgDb
    [b, bb]
    [
        Comment {thread = b, text = "Some b text 1"},
        Comment {thread = b, text = "Some b text 2"},
        Comment {thread = bb, text = "Some bb text 1"},
        Comment {thread = bb, text = "Some bb text 2"},
        Comment {thread = bb, text = "Some bb text 3"}
    ]

frontend :: BootstrapFrontend
frontend = BootstrapFrontend 

main :: IO ()
main = run db frontend
