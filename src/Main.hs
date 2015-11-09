
module Main where

--import Network.Socket.ByteString (send, recv)
import Network.Simple.TCP
import Control.Monad (forever)
import Control.Concurrent.Async

main :: IO ()
main = withSocketsDo $ do
    connect "irc.freenode.net" "6667" $ \(connectionSocket, remoteAddr) -> do
        putStrLn $ "Connection Established to " ++ show remoteAddr
        withAsync (printIncoming connectionSocket) $ \a -> do
            terminalLine <- getLine
            putStrLn "Now Exiting."

printIncoming :: Socket -> IO ()
printIncoming socket = forever $ do
    msg <- recv socket 512
    print msg
