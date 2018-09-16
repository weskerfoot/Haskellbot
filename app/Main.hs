module Main where

import Lib
import Types
import IRCParser (runParseMessage)

import Control.Applicative
import Network.Socket hiding (recv)
import Network.Socket.ByteString as S
import qualified Data.ByteString.Lazy as L
import Control.Concurrent (threadDelay, forkIO)

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
  print $ runParseMessage msg
  threadDelay 1000000
  recvLoop sock

sendIRCConnect host = do
  addr <- getAddress host
  sock <- getIRCSock addr
  connect sock (addrAddress addr)
  forkIO $ recvLoop sock
  threadDelay 1000000
  sendMany sock (buildCommand <$> [userCmd, nickCmd, joinCmd, initMsgCmd])

main = undefined
