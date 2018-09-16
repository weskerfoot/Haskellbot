module Types where

import qualified Data.Text as T

data Message = Message {
                ircCommand :: T.Text,
                ircCommandArgs :: [T.Text]
              } deriving (Show)
