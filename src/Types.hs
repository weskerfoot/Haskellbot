module Types where

import qualified Data.Text as T

data Source = Source {
                sourceNick :: T.Text,
                sourceUsername :: T.Text,
                sourceHostmask :: T.Text
              } deriving (Show)

data Message = Message {
                ircSource :: Maybe Source,
                ircCommand :: T.Text,
                ircCommandArgs :: [T.Text]
              } deriving (Show)
