module Lib (handleSessionRequest) where

import qualified Parser as P
import Files (getFiles)

--import           Data.Default
import           System.Exit

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as C8 --hiding (putStrLn, unsnoc, snoc, map, filter, concat,head)

import qualified Data.Text                     as T 
import qualified Data.Text.Encoding            as TE
import qualified Data.List as L

import           Network.SSH
import           Network.SSH.Server            (SessionHandler, SessionHandler(..)) 


handleSessionRequest :: (Show user) => state -> user -> IO (Maybe SessionHandler)
handleSessionRequest state user = pure $ Just $ SessionHandler $ sessionHandler state user BS.empty

sessionHandler :: (InputStream stdin, OutputStream stdout) => state -> user -> BS.ByteString -> Environment -> Maybe TermInfo -> Maybe Command -> stdin -> stdout -> stderr -> IO ExitCode     
sessionHandler _ _ previousCommandBytes _ _ _ stdin stdout _ = do
  introHandler stdout 
--  content <- getContent
  recurrentSessionHandler previousCommandBytes stdin stdout

introHandler :: OutputStream stdout => stdout -> IO ()
introHandler stdout = sendAll stdout $ C8.pack $ packageOutput introText

introText :: String
introText = "Welcome! I'm aryzach and this is my custom SSH server. There's not much here now, but I made this as a little playground to learn and host some of the things I make."

recurrentSessionHandler :: (InputStream stdin, OutputStream stdout) => BS.ByteString -> stdin -> stdout -> IO ExitCode     
recurrentSessionHandler previousCommandBytes stdin stdout = do
    p <- receive stdin 1024
    let currentCommandBytes = (BS.append previousCommandBytes p)
    sendAll stdout p
    case P.parse' (C8.unpack currentCommandBytes) of
      Left _ -> recurrentSessionHandler currentCommandBytes stdin stdout  
      Right command -> do
        cr <- handleCommand command 
        case cr of 
          ES   -> pure ExitSuccess
          CR s -> sendOutput stdout s >> recurrentSessionHandler BS.empty stdin stdout 
          OtherAction s -> putStrLn "new LINE" >> sendOutput stdout s >> recurrentSessionHandler BS.empty stdin stdout 


      
handleCommand :: P.Command -> IO CommandResponse
handleCommand P.Esc = pure ES 
handleCommand P.InvalidCommand = return $ OtherAction "\n"
handleCommand P.Ls = do
  f <- getFiles
  return $ CR $ L.intercalate " " f
handleCommand (P.Cat s) = do
  f <- getFiles 
  if elem s f 
  then do
    contents <- readFile s
    return $ CR contents
  else return $ OtherAction "no such file"

data CommandResponse = ES | CR String | OtherAction String

sendOutput :: (OutputStream stdout) => stdout -> String -> IO ()
sendOutput stdout s = sendAll stdout $ C8.pack $ packageOutput s

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

createResponseContent :: String -> String
createResponseContent userCommand = case stripAll userCommand of
  "ls" -> "README.md Projects Desktop"
  "cd" -> "new dir"
  _    -> "bad command"

stripAll :: String -> String
stripAll s = filter (\char -> char /= '\n' && char /= '\r') s


