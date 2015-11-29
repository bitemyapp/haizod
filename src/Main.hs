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
import Control.Monad.State

import Haizod.IRC.Parse
import Haizod.IRC.Data

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

isPing :: Message -> Bool
isPing (Unsourced (Ping m)) = True
isPing _ = False

-- | Prints out all the incoming data from the socket. Once a "PING" shows up
--   a "QUIT" message is sent and then the socket is closed and the function returns.
printIncoming :: Socket -> IO ()
printIncoming socket = printIncoming' ""
    where printIncoming' oldSpare = do
            msg <- recv socket 512
            case msg of
                Just bytes -> do
                    let (messageList, newSpare) = runState (parseBytes bytes) oldSpare
                    if null messageList
                        then printIncoming' newSpare
                        else do
                            mapM_ print messageList
                            case filter isPing messageList of
                                [] -> printIncoming' newSpare
                                _ -> do
                                    let reply = "QUIT :done\r\n"
                                    B.putStr (">" `B.append` reply)
                                    send socket reply
                                    closeSock socket
                                    return ()
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
