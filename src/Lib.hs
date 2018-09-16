{-# LANGUAGE OverloadedStrings #-}

module Lib where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.ByteString
import Types

user name desc = Message "USER" [" ", name, " * * :", desc]

nick name = Message "NICK" [" ", name]

join channel = Message "JOIN" [" ", channel]

privmsg channel message = Message "PRIVMSG" [" ", channel, " :", message]

quit = Message "QUIT" []

buildCommand :: Message -> ByteString
buildCommand (Message cmd args) = TE.encodeUtf8 $ (cmd `T.append` (mconcat args)) `T.append` "\r\n"
