{-# LANGUAGE OverloadedStrings #-}

module Main where

--import Network.Socket.ByteString (send, recv)
import Network.Simple.TCP
import Control.Exception (bracket_)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.ByteString as B

import Haizod.IRC.Parse

-- | Holds the data for an IRC Server. Will probably have more
--   fields later, but for now it's just name and port number.
data ServerData = ServerData {
    getServerName :: String,
    getServerPort :: Int
    } deriving (Eq, Ord, Show)

-- | Just a more firnedly name for a simple constant.
oneSecondInMicroseconds :: Int
oneSecondInMicroseconds = 1000000

-- | Connects to a server, puts that server into the TVar, and manages IO with
--   that server until it's done. At the end, the server is removed from the TVar.
--   the 'finally' function is used to ensure that the server gets removed from the
--   TVar even if the connection processing has an exception.
serverConnect :: ServerData -> TVar (Set ServerData) -> IO ()
serverConnect servdat connectionsTVar = connect (getServerName servdat) (show . getServerPort $ servdat) $
    \(socket, remoteAddr) -> bracket_
        (atomically $ modifyTVar' connectionsTVar (\connections -> S.insert servdat connections))
        (atomically $ modifyTVar' connectionsTVar (\connections -> S.delete servdat connections))
        $ do
            putStrLn $ "Connection Established to " ++ show remoteAddr
            let nick = "NICK haizod\r\n"
            let user = "USER haizod 0 * :haizod\r\n"
            B.putStr (">" `B.append` nick)
            send socket nick
            B.putStr (">" `B.append` user)
            send socket user
            printIncoming socket

-- | A simple IO action that takes a TVar of what servers are connected and
--   waits until there are no servers left.
waitToQuit :: TVar (Set ServerData) -> IO ()
waitToQuit connectionsTVar = atomically $ do
    connections <- readTVar connectionsTVar
    if S.null connections
        then return ()
        else retry

-- | Prints out all the incoming data from the socket. Once a "PING" shows up
--   a "QUIT" message is sent and then the socket is closed and the function returns.
printIncoming :: Socket -> IO ()
printIncoming socket = do
    msg <- recv socket 512
    case msg of
        Just s -> do
            B.putStr s
            if B.isPrefixOf "PING " s
                then do
                    --let reply = "PONG " `B.append` (B.drop 5 s)
                    let reply = "QUIT :exiting.\r\n"
                    B.putStr (">" `B.append` reply)
                    send socket reply
                    return ()
                else printIncoming socket
        Nothing -> do
            print "Socket closed."

-- | Runs the bot. Right now just a single server is connected to.
main :: IO ()
main = withSocketsDo $ do
    let servData = ServerData "irc.freenode.net" 6667
    connectionsTVar <- atomically $ newTVar S.empty
    forkIO $ serverConnect servData connectionsTVar
    -- We delay to give the first connection time to open before waiting to quit.
    threadDelay oneSecondInMicroseconds
    waitToQuit connectionsTVar
