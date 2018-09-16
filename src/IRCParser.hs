{-# LANGUAGE OverloadedStrings #-}
module IRCParser where

import Data.ByteString (pack)
import Data.Attoparsec.ByteString
import Control.Applicative

-- :nisstyre!wes@oftn/oswg-member/Nisstyre PRIVMSG #thisisatestwhatever :yay\r\n
-- :nisstyre!wes@oftn/oswg-member/Nisstyre PART #thisisatestwhatever :\"WeeChat 2.2\"\r\n
-- :nisstyre!wes@oftn/oswg-member/Nisstyre JOIN #thisisatestwhatever\r\n
-- :nisstyre!wes@oftn/oswg-member/Nisstyre QUIT :Quit: WeeChat 2.2\r\n
-- PING :asimov.freenode.net\r\n

space = word8 32
notSpace = notWord8 32

-- 13 = \r, 10 = \n
crlf c = c == 13 || c == 10

-- :
colon = word8 58

command = choice $ map string ["PRIVMSG", "PART", "JOIN", "QUIT", "PING"]

argument = takeWhile1 (\c -> c /= 58 && c /= 32)

parseFinalArg = do
  _ <- colon
  command <- takeWhile1 (not . crlf)
  return command

parseArgs = do
  args <- (argument `sepBy` space) <|> alwaysMatch []
  _ <- space <|> (alwaysMatch 0)
  finalArg <- parseFinalArg <|> (alwaysMatch "")
  return (args, finalArg)

source = do
  _ <- colon
  source <- manyTill notSpace space
  return source

-- Matches any character and does not consume input
-- Returns value passed in
alwaysMatch x = (maybe x (const x)) <$> peekWord8

parseMessage = do
  sourceName <- source <|> (alwaysMatch [])
  commandName <- command
  _ <- space
  arguments <- parseArgs
  return (pack sourceName, commandName, arguments)

runParseMessage = parseOnly parseMessage
