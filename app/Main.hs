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

main :: IO ()
main = do
  --file                <- BS.readFile "./resources/id_rsa"
  --(privateKey, _) : _ <- decodePrivateKeyFile BS.empty file :: IO [(KeyPair, BA.Bytes)]
  privateKey <- newKeyPair
  Server.runServer config privateKey
  --bracket open close (accept config privateKey)
    where
        config = def
                { Server.socketConfig             = def { Server.socketBindAddresses = pure (Address "172.31.14.100" 2024)}
                , Server.transportConfig          = def 
                        { onSend = \x -> putStrLn ("CLIENT: " ++ show x)
                        , onReceive = \x -> putStrLn ("SERVER: " ++ show x)
                        }
                , Server.userAuthConfig           = def
                        { Server.onAuthRequest        = \_ username _ _ _ -> pure (Just username)
                        }
                , Server.connectionConfig         = def
                        { Server.onSessionRequest     = handleSessionRequest
                        --, Server.onDirectTcpIpRequest = handleDirectTcpIpRequest
                        }
                , Server.onConnect                = \_ -> do pure (Just ())
                }
        {-
        open  = S.socket :: IO (S.Socket S.Inet S.Stream S.Default)
        close = S.close
        accept config agent s = do
                S.setSocketOption s (S.ReuseAddress True)
                --S.setSocketOption s (S.V6Only False)
                S.bind s (S.SocketAddressInet S.inetLoopback 2200)
                S.listen s 5
                forever $ bracketOnError (S.accept s) (S.close . fst) $ \(stream, peer) -> do
                        putStrLn $ "Connection from " ++ show peer
                        void $ forkFinally
                                (Server.serve config agent stream)
                                (const $ S.close stream)
                                -}

{-
handleDirectTcpIpRequest :: identity -> DirectTcpIpRequest -> IO (Maybe Server.DirectTcpIpHandler)
handleDirectTcpIpRequest idnt req = pure $ Just $ Server.DirectTcpIpHandler $ \stream-> do
  bs <- receive stream 4096
  sendAll stream "HTTP/1.1 200 OK\n"
  sendAll stream "Content-Type: text/plain\n\n"
  sendAll stream $! BS.pack $ fmap (fromIntegral . fromEnum) $ show req
  sendAll stream "\n\n"
  sendAll stream bs
  print bs
  -}

{-
handleSessionRequest :: identity -> Server.SessionRequest -> IO (Maybe Server.SessionHandler)
handleSessionRequest idnt req = pure $ Just $ Server.SessionHandler $ \_ _ _ _ stdout _ -> do
    sendAll stdout "Hello world!\n"
    pure ExitSuccess
    -}

handleSessionRequest :: (Show user) => state -> user -> IO (Maybe Server.SessionHandler)
handleSessionRequest state user = pure $ Just $ Server.SessionHandler $ mySessionHandler state user BS.empty
  
mySessionHandler :: (Show user, InputStream stdin, OutputStream stdout, OutputStream stderr) => state -> user -> BS.ByteString -> Environment -> Maybe TermInfo -> Maybe Command -> stdin -> stdout -> stderr -> IO ExitCode     
mySessionHandler state user previousCommandBytes a b c stdin stdout d = do
    p <- receive stdin 1024
    let currentCommandBytes = (BS.append previousCommandBytes p)
    sendAll stdout p
    if lastIsCarriageReturn $ TE.decodeUtf8 p
      then do
        sendAll stdout $ C8.pack . createResponseFromCommand . C8.unpack $ currentCommandBytes
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


