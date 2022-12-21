module Lib (handleSessionRequest) where

import qualified Parser as P
import Files (files)

import           System.Exit

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as C8 

import qualified Data.Text                     as T 
import qualified Data.Text.Encoding            as TE
import qualified Data.List as L

import           Network.SSH
import           Network.SSH.Server            (SessionHandler, SessionHandler(..)) 

import qualified Data.Map as M

handleSessionRequest :: (Show user) => state -> user -> IO (Maybe SessionHandler)
handleSessionRequest state user = pure $ Just $ SessionHandler $ sessionHandler state user BS.empty

sessionHandler :: (InputStream stdin, OutputStream stdout) => state -> user -> BS.ByteString -> Environment -> Maybe TermInfo -> Maybe Command -> stdin -> stdout -> stderr -> IO ExitCode     
sessionHandler _ _ previousCommandBytes _ _ _ stdin stdout _ = do
  introHandler stdout 
  recurrentSessionHandler previousCommandBytes stdin stdout

introHandler :: OutputStream stdout => stdout -> IO ()
introHandler stdout = sendAll stdout $ C8.pack $ packageOutput introText

introText :: String
introText = L.intercalate "\n\r\n\r" 
  [ "Welcome!" 
  , "I'm aryzach and this is a custom SSH server. There's not much here now." 
  , "This is inspired by github.com/hackclub/jobs (ssh jobs.hackclub.com)"
  , "I built this to learn how it was done" 
  , "fyi I'm handling raw keystrokes, so expect some strange behavior! (ex. 'backspace' doesn't behave as generally expected)"
  , "I also haven't thought about any security issues around this / self-hosting, so please don't put it through the ringer yet..."
  , packageOutput help
  ]

help :: String
help = L.intercalate "\n\r" 
  [ "ls"
  , "cat [file]"
  , "Ctrl-C to exit"
  ]

recurrentSessionHandler :: (InputStream stdin, OutputStream stdout) => BS.ByteString -> stdin -> stdout -> IO ExitCode     
recurrentSessionHandler previousCommandBytes stdin stdout = do
    p <- receive stdin 1024
    if not $ P.isValidKey $ C8.head p
    then recurrentSessionHandler previousCommandBytes stdin stdout  
    else do
      let currentCommandBytes = (BS.append previousCommandBytes p)
      sendAll stdout p
      case P.parse' (C8.unpack currentCommandBytes) of
        Left _ -> recurrentSessionHandler currentCommandBytes stdin stdout  
        Right command -> do
          case handleCommand command of 
            ES   -> pure ExitSuccess
            CR s -> sendOutput stdout s >> recurrentSessionHandler BS.empty stdin stdout 
            OtherAction s -> sendOutput stdout s >> recurrentSessionHandler BS.empty stdin stdout 
            InvalidKey -> putStrLn "invalid key" >> recurrentSessionHandler BS.empty stdin stdout 
      
handleCommand :: P.Command -> CommandResponse
handleCommand P.Esc = ES 
handleCommand P.InvalidCommand = OtherAction "invalid command"
handleCommand (P.InvalidKey k) = InvalidKey
handleCommand P.Ls = CR $ L.intercalate " " $ M.keys files
handleCommand (P.Cat s) = case M.lookup s files of
  Just c -> CR c
  Nothing -> OtherAction "no such file"

data CommandResponse = ES | CR String | OtherAction String | InvalidKey

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


