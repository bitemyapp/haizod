
module Haizod.IRC.Data where

import Data.Text

-- | Holds the data for an IRC Server. Will probably have more
--   fields later, but for now it's just name and port number.
data ServerData = ServerData {
    getServerName :: String,
    getServerPort :: Int
    } deriving (Eq, Ord, Show)

data Message = Sourced Source Command
             | Unsourced Command
             deriving (Eq, Show)

data Source = Server Text
            | Client Text (Maybe Text) (Maybe Text) -- ^ Client Nickname Username Hostname
            deriving (Eq, Show)

data Command = Ping Text
             | Pong Text
             | PrivMsg Text Text -- ^ PrivMsg Channel MessageBody
             | Notice Text Text -- ^ Notice Nick MessageBody
             | Mode Text Text [Text] -- ^ Mode Target Operation [ExtraParams]
             | Numb Text [Text]
             | Other Text [Text]
             deriving (Eq, Show)

-- | If the message contains a Ping or not.
isPing :: Message -> Bool
isPing (Unsourced (Ping _)) = True
isPing (Sourced _ (Ping _)) = True
isPing _ = False
