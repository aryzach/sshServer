{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Default
import           Network.SSH
import           Network.SSH.Server            (runServer, socketConfig, transportConfig, userAuthConfig, onAuthRequest, connectionConfig, onSessionRequest, socketBindAddresses, onConnect, SessionHandler, SessionHandler(..)) 
import Crypto.PubKey.Ed25519

import qualified Lib as L

--ipAddress = "172.31.14.100"
ipAddress = "0.0.0.0"
port = 22

main :: IO ()
main = do
  privateKey <- newKeyPair
  runServer config privateKey
    where
        config = def
                { socketConfig             = def { socketBindAddresses = pure (Address ipAddress port)}
                , transportConfig          = def 
                        { onSend = \x -> putStrLn ("CLIENT: " ++ show x)
                        , onReceive = \x -> putStrLn ("SERVER: " ++ show x)
                        }
                , userAuthConfig           = def
                        { onAuthRequest        = \_ username _ _ _ -> pure (Just username)
                        }
                , connectionConfig         = def
                        { onSessionRequest     = L.handleSessionRequest
                        }
                , onConnect                = \_ -> do pure (Just ())
                }

