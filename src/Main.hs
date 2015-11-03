
module Main where

import Network.Simple.TCP

main :: IO ()
main = withSocketsDo $ do
    connect "irc.freenode.net" "6667" $ \(connectionSocket, remoteAddr) -> do
        putStrLn $ "Connection Established to " ++ show remoteAddr
        putStrLn "Now Exiting."
