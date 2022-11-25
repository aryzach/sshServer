{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Network.Socket
import qualified Network.Socket.ByteString      as SBS
import           Control.Concurrent             ( forkFinally
    )
import           Control.Exception              ( bracket
    , bracketOnError
    , handle
    , throwIO
    )
import           Control.Monad                  ( forever
    , void
    )
import qualified Data.ByteArray                as BA
import qualified Data.ByteString               as BS
import           Data.Default
import           System.Exit

import qualified System.Socket                  as S
import qualified System.Socket.Family.Inet      as S
import qualified System.Socket.Protocol.Default as S
import qualified System.Socket.Type.Stream      as S
import qualified System.Socket.Unsafe           as S

import           Network.SSH
import qualified Network.SSH.Server            as Server

import           Data.Text                     as T hiding (map, filter)
import           Data.Text.Encoding            as TE

import           Data.ByteString.Char8         as C8 hiding (putStrLn, unsnoc, snoc, map, filter)

-- tui
import Graphics.Vty
import System.Posix.Types
import System.Posix.Terminal
import System.Posix.IO.ByteString
import System.Environment (lookupEnv)

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
-- tui


main :: IO ()
main = do
  privateKey <- newKeyPair
  Server.runServer config privateKey
    where
        config = def
                { Server.socketConfig             = def { Server.socketBindAddresses = pure (Address "172.31.14.100" 2023)}
                , Server.transportConfig          = def 
                        { onSend = \x -> putStrLn ("CLIENT: " ++ show x)
                        , onReceive = \x -> putStrLn ("SERVER: " ++ show x)
                        }
                , Server.userAuthConfig           = def
                        { Server.onAuthRequest        = \_ username _ _ _ -> pure (Just username)
                        }
                , Server.connectionConfig         = def
                        { Server.onSessionRequest     = handleSessionRequest
                        }
                , Server.onConnect                = \_ -> do pure (Just ())
                }
    
handleSessionRequest :: (Show user) => state -> user -> IO (Maybe Server.SessionHandler)
handleSessionRequest state user = pure $ Just $ Server.SessionHandler $ mySessionHandler state user BS.empty
  
mySessionHandler :: (Show user, InputStream stdin, OutputStream stdout, OutputStream stderr) => state -> user -> BS.ByteString -> Environment -> Maybe TermInfo -> Maybe Command -> stdin -> stdout -> stderr -> IO ExitCode     
mySessionHandler state user previousCommandBytes a b c stdin stdout d = do
    userInput <- receive stdin 1024

    let currentCommandBytes = (BS.append previousCommandBytes userInput)
        line0 = string (defAttr ` withForeColor ` green) "first line"
        line1 = string (defAttr ` withBackColor ` blue) "second line"
        img = line0 <-> line1
        pic = picForImage img
        picAsBytes = C8.pack $ TL.unpack pic

    sendAll stdout userInput
    if lastIsCarriageReturn $ TE.decodeUtf8 userInput
      then do
        sendAll stdout $ C8.pack . createResponseFromCommand . C8.unpack $ currentCommandBytes
        sendAll stdout picAsBytes
        mySessionHandler state user BS.empty a b c stdin stdout d 
      else do
        mySessionHandler state user currentCommandBytes a b c stdin stdout d 

lastIsCarriageReturn :: T.Text -> Bool
lastIsCarriageReturn t = case unsnoc t of
  Just (_,l) -> l == '\r'
  Nothing -> False

handleBSCommand :: BS.ByteString -> BS.ByteString
handleBSCommand bs = bs

createResponseFromCommand :: String -> String
createResponseFromCommand text = Prelude.concat $
  [ 
    "\n"
  , "\r"
  , createResponseContent $ text
  , "\n"
  , "\r"
  ]

createResponseContent :: String -> String
createResponseContent userCommand = case isUserCommand (stripAll userCommand) of
  Just LS -> "Documents Projects Desktop"
  Just CD -> "new dir"
  Nothing -> "bad command"

data UserCommand = LS | CD

isUserCommand :: String -> Maybe UserCommand
isUserCommand "ls" = Just LS
isUserCommand "cd" = Just CD
isUserCommand _    = Nothing

stripAll :: String -> String
stripAll s = filter (\char -> char /= '\n' && char /= '\r') s

{-

     (r,w) <- openPseudoTerminal
     cfg <- configWithPipe stdInput stdOutput
     vty <- mkVty cfg
     let line0 = string (defAttr ` withForeColor ` green) "first line"
         line1 = string (defAttr ` withBackColor ` blue) "second line"
         img = line0 <-> line1
         pic = picForImage img
     update vty pic
     e <- nextEvent vty
     shutdown vty
     print ("Last event was: " ++ show e)
     putStrLn $ show img


configWithPipe :: Fd -> Fd -> IO Config
configWithPipe readPipe writePipe = do
    -- this should instead be from TermInfo on the client I think. This is a temp solution
    mb <- lookupEnv "TERM"
    print mb
    case mb of
      Nothing -> standardIOConfig
      Just t -> do
        return defaultConfig
          { vmin               = Just 1
          , mouseMode          = Just False
          , bracketedPasteMode = Just False
          , vtime              = Just 100
          , inputFd            = Just readPipe
          , outputFd           = Just writePipe
          , termName           = Nothing
          }

-}
