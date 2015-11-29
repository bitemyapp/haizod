{-# LANGUAGE OverloadedStrings #-}

module Main where

--import Network.Socket.ByteString (send, recv)
import Data.ByteString (putStr, isPrefixOf, append, drop)
import Network.Simple.TCP
import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Concurrent.STM
import qualified Data.Set as S
import Data.Set (Set)

data ServerData = ServerData {
    getServerName :: String,
    getServerPort :: Int
    } deriving (Eq, Ord, Show)

serverConnect :: ServerData -> TVar (Set ServerData) -> IO ()
serverConnect servdat connectionsTVar = connect (getServerName servdat) (show . getServerPort $ servdat) $
    \(socket, remoteAddr) -> do
        atomically $ modifyTVar' connectionsTVar (\connections -> S.insert servdat connections)
        putStrLn $ "Connection Established to " ++ show remoteAddr
        send socket "NICK haizod\r\n"
        send socket "USER haizod 0 * :haizod\r\n"
        printIncoming socket
        atomically $ modifyTVar' connectionsTVar (\connections -> S.delete servdat connections)

main :: IO ()
main = withSocketsDo $ do
    let servData = ServerData "irc.freenode.net" 6667
    connectionsTVar <- atomically $ newTVar S.empty
    withAsync (serverConnect servData connectionsTVar) $ \a -> do
        threadDelay 1000000 -- We delay to give the first connection time to open before waiting.
        waitToQuit connectionsTVar

waitToQuit :: TVar (Set ServerData) -> IO ()
waitToQuit connectionsTVar = atomically $ do
    connections <- readTVar connectionsTVar
    if S.null connections
        then return ()
        else retry

printIncoming :: Socket -> IO ()
printIncoming socket = do
    msg <- recv socket 512
    case msg of
        Just s -> do
            Data.ByteString.putStr s
            if Data.ByteString.isPrefixOf "PING " s
                then do
                    --let reply = "PONG " `Data.ByteString.append` (Data.ByteString.drop 5 s)
                    let reply = "QUIT :exiting.\r\n"
                    Data.ByteString.putStr (">" `Data.ByteString.append` reply)
                    send socket reply
                    return ()
                else printIncoming socket
        Nothing -> do
            print "Socket closed."
