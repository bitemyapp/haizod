
module Haizod.IRC.Data where

import Data.Text

data Message = Sourced Source Command
             | Unsourced Command
             deriving (Eq, Show)

data Source = Server Text
            | Client Text (Maybe Text) (Maybe Text) -- ^ Client Nickname Username Hostname
            deriving (Eq, Show)

data Command = Ping Text
             | Pong Text
             | PrivMsg Text Text -- ^ PrivMsg Channel MessageBody
             | Other Text
             deriving (Eq, Show)
