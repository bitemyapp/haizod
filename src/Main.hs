{-# LANGUAGE OverloadedStrings #-}

module Main where

--import Network.Socket.ByteString (send, recv)
import Data.ByteString (putStr)
import Network.Simple.TCP
import Control.Monad (forever)
import Control.Concurrent.Async

main :: IO ()
main = withSocketsDo $ do
    connect "irc.freenode.net" "6667" $ \(connectionSocket, remoteAddr) -> do
        putStrLn $ "Connection Established to " ++ show remoteAddr
        send connectionSocket "NICK haizod \r\n"
        send connectionSocket "USER haizod 0 * :haizod\r\n"
        withAsync (printIncoming connectionSocket) $ \a -> do
            terminalLine <- getLine
            putStrLn "Now Exiting."

printIncoming :: Socket -> IO ()
printIncoming socket = do
    msg <- recv socket 512
    case msg of
        Just s -> do
            Data.ByteString.putStr s
            printIncoming socket
        Nothing -> do
            print "No more data, closing."
