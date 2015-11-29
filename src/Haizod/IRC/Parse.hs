{-# LANGUAGE OverloadedStrings #-}

module Haizod.IRC.Parse where

import Haizod.IRC.Data
import Data.ByteString (ByteString)
--import qualified Data.Bytestring as B
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding as E
import Data.Maybe (catMaybes)
import Control.Monad.State
import Data.Char (isDigit)

-- | Given a new ByteString, you get a State that will return
--   a list of the Messages and keep track of the extra Text
--   that's left over.
parseBytes :: ByteString -> State Text [Message]
parseBytes bytes = do
    let newText = E.decodeUtf8 bytes
    storedText <- get
    let combined = storedText `T.append` newText
    if lineMarker `T.isInfixOf` combined
        then do
            let splitList = T.splitOn lineMarker combined
            let messages = catMaybes $ map parseLine (init splitList)
            let spare = last splitList
            put spare
            return messages
        else do
            put combined
            return []

-- | IRC lines have "\r\n" between each of them.
lineMarker :: Text
lineMarker = "\r\n"

-- | If the Text is a textual version of an integer. Used for the
--   numeric command codes.
isInteger :: Text -> Bool
isInteger = T.all isDigit

-- | Given 'needle' and 'haystack', seperates the haystack value into all the
--   text before the needle, all the text after it. If the needle isn't in the
--   haystack then the second value will be null. The needle must not be null.
splitOnce :: Text -> Text -> (Text, Text)
splitOnce needle haystack = let (val, rest) = T.breakOn needle haystack
    in case T.stripPrefix needle rest of
        Just suff -> (val, suff)
        Nothing -> (val, "")

-- | Given a ByteString, parses out as many messages as possible, giving back
--   the list of results and the remaining ByteString.
parseBytesOld :: ByteString -> [Message]
parseBytesOld serverBytes = out
    where serverText = E.decodeUtf8 serverBytes
          lineList = T.splitOn lineMarker serverText
          out = catMaybes $ map parseLine (filter (not . T.null) lineList)

-- | Parses a single line from the server into its Message value.
parseLine :: Text -> Maybe Message
parseLine line = case T.stripPrefix ":" line of
    Just suff -> do
        let (sourceText, commandText) = splitOnce " " suff
        source <- parseSource sourceText
        command <- parseCommand (T.stripStart commandText)
        return $ Sourced source command
    Nothing -> do
        command <- parseCommand line
        return $ Unsourced command

-- | Parses the source segment of a line into a proper Source value.
parseSource :: Text -> Maybe Source
parseSource sourceText = Just $ if "." `T.isInfixOf` sourceText
    then Server sourceText
    else let (nick, rest) = splitOnce "!" sourceText in
        if T.null rest
            then Client nick Nothing Nothing
            else let (user, host) = splitOnce "@" rest in
                if T.null host
                    then Client nick (Just rest) Nothing
                    else Client nick (Just user) (Just host)

-- | Parses the command segment of a line into a proper Command value.
parseCommand :: Text -> Maybe Command
parseCommand t = case parseTokens t of
    (command : args) -> case command of
        "PING" -> case args of
            [arg] -> Just (Ping arg)
            _ -> Nothing
        "PONG" -> case args of
            [arg] -> Just (Pong arg)
            _ -> Nothing
        "PRIVMSG" -> case args of
            [chanName, messageBody] -> Just (PrivMsg chanName messageBody)
            _ -> Nothing
        "NOTICE" -> case args of
            [nick, messageBody] -> Just (Notice nick messageBody)
            _ -> Nothing
        "MODE" -> case args of
            [target,op,limit,user,banMask] -> Just $ Mode target op [limit,user,banMask]
            [target,op,limit,user] -> Just $ Mode target op [limit,user]
            [target,op,limit] -> Just $ Mode target op [limit]
            [target,op] -> Just $ Mode target op []
            _ -> Nothing
        _ -> if isInteger command
            then Just $ Numb command args
            else Just $ Other command args
    [] -> Nothing

-- | Parses the tokens of a command. Each is seperated by one or more spaces,
--   except that the final token can be prefixed with a ':', in which case
--   all text after that is a single token, including the spaces.
parseTokens :: Text -> [Text]
parseTokens t = case T.stripPrefix ":" t of
    Just rest -> [rest]
    Nothing -> let (val, more) = splitOnce " " t in
        if T.null more
            then [val]
            else val : parseTokens (T.stripStart more)
