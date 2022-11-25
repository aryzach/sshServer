{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Default
import           System.Exit

import qualified Data.ByteString               as BS
import           Data.ByteString.Char8         as C8 hiding (putStrLn, unsnoc, snoc, map, filter, concat)

import qualified Data.Text                     as T 
import qualified Data.Text.Encoding            as TE

import           Network.SSH
import           Network.SSH.Server            (runServer, socketConfig, transportConfig, userAuthConfig, onAuthRequest, connectionConfig, onSessionRequest, socketBindAddresses, onConnect, SessionHandler, SessionHandler(..)) 


main :: IO ()
main = do
  privateKey <- newKeyPair
  runServer config privateKey
    where
        config = def
                { socketConfig             = def { socketBindAddresses = pure (Address "172.31.14.100" 2024)}
                , transportConfig          = def 
                        { onSend = \x -> putStrLn ("CLIENT: " ++ show x)
                        , onReceive = \x -> putStrLn ("SERVER: " ++ show x)
                        }
                , userAuthConfig           = def
                        { onAuthRequest        = \_ username _ _ _ -> pure (Just username)
                        }
                , connectionConfig         = def
                        { onSessionRequest     = handleSessionRequest
                        }
                , onConnect                = \_ -> do pure (Just ())
                }


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
    case T.unsnoc (TE.decodeUtf8 p) of
      Just (_,'\r') -> (sendAll stdout $ C8.pack . createResponseFromCommand . C8.unpack $ currentCommandBytes) >> recurrentSessionHandler BS.empty stdin stdout 
      Just (_,'\ETX') -> pure ExitSuccess
      _ -> recurrentSessionHandler currentCommandBytes stdin stdout  


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

data File = Readme | Contact 
instance Show File where
	show Readme = "README.md"
	show Contact = "contact.md"

data Programs = Writeme | Cat File | Ls

--use bitnomial parser to handle incoming commands here

createResponseContent :: String -> String
createResponseContent userCommand = case stripAll userCommand of
  "ls" -> "README.md Projects Desktop"
  "cd" -> "new dir"
  _    -> "bad command"

stripAll :: String -> String
stripAll s = filter (\char -> char /= '\n' && char /= '\r') s


