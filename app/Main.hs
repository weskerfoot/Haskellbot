{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Types
import IRCParser (runParseMessage)

import Control.Applicative
import Network.Socket hiding (recv)
import Network.Socket.ByteString as S
import qualified Data.ByteString.Lazy as L
import Control.Concurrent (threadDelay, forkIO)
import Data.Text (isInfixOf)

getNick (Message source _ _) = maybe "" id (sourceNick <$> source)

react sock msg = do
  case (ircCommand msg) of
    "PING" -> sendMany sock (buildCommand <$> [pong (head $ ircCommandArgs msg)])
    "PRIVMSG" -> if ("crapbot" `isInfixOf` (head $ tail $ ircCommandArgs msg)) then
                        sendMany sock
                          (buildCommand <$>
                            [privmsg (head $ ircCommandArgs msg) (mconcat ["Hello there ", getNick msg, "!"])])
                else
                  return ()

    command -> print command
  return ()

channel = "#thisisatestwhatever"

userCmd = user "crapbot" "A crappy bot"
nickCmd = nick "crapbot"
joinCmd = join channel
initMsgCmd = privmsg channel "Hello, my fellow humans!"

getAddress hname = head <$>
                   getAddrInfo (return defaultHints) (return hname) (return "6667")

getIRCSock addr = socket (addrFamily addr) Stream defaultProtocol

recvLoop sock = do
  msg <- recv sock 4096
  case runParseMessage msg of
    Left _ -> return ()
    Right msg -> do
                  _ <- react sock msg
                  print msg
  threadDelay 1000000
  recvLoop sock

sendIRCConnect host = do
  addr <- getAddress host
  sock <- getIRCSock addr
  connect sock (addrAddress addr)
  forkIO $ recvLoop sock
  threadDelay 1000000
  sendMany sock (buildCommand <$> [userCmd, nickCmd, joinCmd, initMsgCmd])

main = do
  _ <- sendIRCConnect "irc.freenode.org"
  _ <- getLine
  return ()
