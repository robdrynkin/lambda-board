{-# LANGUAGE OverloadedStrings #-}

module BootstrapFrontend where

import           FrontendBase
import           Lib

import           Data.List
import           Data.Text           (concat, pack)


import qualified Data.HashMap.Strict as H
import           Data.List
import           Data.Text           (Text)
import qualified Data.Vector         as V
import           Text.Karver


data BootstrapFrontend = BootstrapFrontend { threadsTemplate :: Text, commentsTemplate :: Text }

instance Frontend BootstrapFrontend where
    allThreadsPage (BootstrapFrontend temp _) threads = let
            content = H.fromList [
                ("threads", List $ V.fromList (map (Literal . name) threads)),
                ("title", Literal "Threads") ]
            in
        renderTemplate content temp

    -- threadPage _ thread comments = Data.Text.concat (map text comments)
    threadPage (BootstrapFrontend _ temp) thread comments = let {
            parseComment comment = Object $ H.fromList [
                ("id", (pack . show . id_) comment),
                ("text", text comment),
                ("replyTo", (pack . show . replyToId) comment)
                -- ("date", date comment)
            ];
            content = H.fromList [
                ("title", Literal (name thread)),
                ("comments", List $ V.fromList $ map parseComment comments)
            ]}
            in
        renderTemplate content temp
