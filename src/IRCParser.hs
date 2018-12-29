{-# LANGUAGE OverloadedStrings #-}
module IRCParser where

import Data.Text (strip)
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString (pack)
import Data.Attoparsec.ByteString
import Control.Applicative
import Data.Either.Combinators (rightToMaybe)
import Types

stripWhitespace = filter $ (all id) . (sequence $ map (/=) [" ", "\r", "\n", ""])

space = word8 32
notSpace = notWord8 32
colon = word8 58 -- :

-- 13 = \r, 10 = \n
crlf c = c == 13 || c == 10

bang = (== 33) -- !
atsign = (== 64) -- @
isSpace = (== 32)

parseNick = takeWhile1 (not . bang)

parseUsername = do
  _ <- word8 33
  username <- takeWhile1 (not . atsign)
  return username

parseHostmask = do
  _ <- word8 64
  hostmask <- takeWhile1 (not . isSpace)
  return hostmask

parseSource = do
  nickname <- parseNick
  username <- parseUsername
  hostmask <- parseHostmask
  return $ Source
            (decodeUtf8 nickname)
            (decodeUtf8 username)
            (decodeUtf8 hostmask)

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
  return $ args ++ [finalArg]

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

  let source = parseOnly parseSource $ pack sourceName

  return $ Message
            (rightToMaybe source)
            (decodeUtf8 commandName)
            (map strip $ stripWhitespace $ map decodeUtf8 arguments)

runParseMessage = parseOnly parseMessage
