{-# LANGUAGE OverloadedStrings #-}

module Lib where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.ByteString
import Types

user name desc = Message Nothing "USER" [" ", name, " * * :", desc]

nick name = Message Nothing "NICK" [" ", name]

join channel = Message Nothing "JOIN" [" ", channel]

privmsg channel message = Message Nothing "PRIVMSG" [" ", channel, " :", message]

pong message = Message Nothing "PONG" [":", message]

quit = Message Nothing "QUIT" []

buildCommand :: Message -> ByteString
buildCommand (Message Nothing cmd args) = TE.encodeUtf8 $ (cmd `T.append` (mconcat args)) `T.append` "\r\n"
buildCommand (Message (Just source) cmd args) = TE.encodeUtf8 $ (cmd `T.append` (mconcat args)) `T.append` "\r\n"
