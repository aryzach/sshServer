module Lib (handleSessionRequest) where

import qualified Parser as P

--import           Data.Default
import           System.Exit

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as C8 --hiding (putStrLn, unsnoc, snoc, map, filter, concat,head)

import qualified Data.Text                     as T 
import qualified Data.Text.Encoding            as TE

import           Network.SSH
import           Network.SSH.Server            (SessionHandler, SessionHandler(..)) 


handleSessionRequest :: (Show user) => state -> user -> IO (Maybe SessionHandler)
handleSessionRequest state user = pure $ Just $ SessionHandler $ sessionHandler state user BS.empty

sessionHandler :: (InputStream stdin, OutputStream stdout) => state -> user -> BS.ByteString -> Environment -> Maybe TermInfo -> Maybe Command -> stdin -> stdout -> stderr -> IO ExitCode     
sessionHandler _ _ previousCommandBytes _ _ _ stdin stdout _ = introHandler stdout >> recurrentSessionHandler previousCommandBytes stdin stdout

introHandler :: OutputStream stdout => stdout -> IO ()
introHandler stdout = sendAll stdout $ C8.pack $ packageOutput introText

introText :: String
introText = "Welcome! I'm aryzach and this is my custom SSH server. There's not much here now, but I made this as a little playground to learn and host some of the things I make."

recurrentSessionHandler :: (InputStream stdin, OutputStream stdout) => BS.ByteString -> stdin -> stdout -> IO ExitCode     
recurrentSessionHandler previousCommandBytes stdin stdout = do
    p <- receive stdin 1024
    let currentCommandBytes = (BS.append previousCommandBytes p)
    sendAll stdout p
    case parseInput (C8.unpack currentCommandBytes) of
      Nothing -> recurrentSessionHandler currentCommandBytes stdin stdout  
      Just P.Esc -> pure ExitSuccess
      Just c     -> (sendAll stdout $ C8.pack . createResponseFromCommand . C8.unpack $ currentCommandBytes) >> recurrentSessionHandler BS.empty stdin stdout 

parseInput :: String -> Maybe P.Command
parseInput i = case (P.parse' i) of
  Left _ -> Nothing
  Right s -> Just s

packageOutput :: String -> String
packageOutput s = concat $
  [ 
    "\n"
  , "\r"
  , s
  , "\n"
  , "\r"
  ]


createResponseFromCommand :: String -> String
createResponseFromCommand = packageOutput . createResponseContent 


--use bitnomial parser to handle incoming commands here

createResponseContent :: String -> String
createResponseContent userCommand = case stripAll userCommand of
  "ls" -> "README.md Projects Desktop"
  "cd" -> "new dir"
  _    -> "bad command"

stripAll :: String -> String
stripAll s = filter (\char -> char /= '\n' && char /= '\r') s


