module Main where

import Lib
import Types

import Control.Applicative
import Network.Socket hiding (recv)
import Network.Socket.ByteString as S
import qualified Data.ByteString.Lazy as L
import Control.Concurrent (threadDelay, forkIO)


channel = "#cockgangsters"

userCmd = buildCommand $ user "crapbot" "A crappy bot"
nickCmd = buildCommand $ nick "crapbot"
joinCmd = buildCommand $ join channel
initMsgCmd = buildCommand $ privmsg channel "Hello, my fellow humans!"
quitCmd = buildCommand quit

getAddress hname = head <$>
                   getAddrInfo (return defaultHints) (return hname) (return "6667")

getIRCSock addr = socket (addrFamily addr) Stream defaultProtocol

recvLoop sock = do
  msg <- recv sock 4096
  print msg
  threadDelay 1000000
  recvLoop sock

sendIRCConnect host = do
  addr <- getAddress host
  sock <- getIRCSock addr
  connect sock (addrAddress addr)
  forkIO $ recvLoop sock
  threadDelay 1000000
  sendMany sock [userCmd, nickCmd, joinCmd, initMsgCmd, quitCmd]

main = undefined
