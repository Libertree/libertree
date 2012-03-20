{-# LANGUAGE OverloadedStrings #-}

module Libertree.Backend.Server where

import Libertree.Backend.Dispatch
import Libertree.Backend.Command -- TODO: remove ref to CmdUnknown
import Libertree.Backend.Request

import Network hiding (accept)
import Network.Socket hiding (send, recv)
import Network.Socket.ByteString (sendAll, recv)
import Control.Concurrent (forkIO)

-- maximum number of bytes to receive
bufferSize :: Int
bufferSize = 1024

-- default port
port :: PortNumber
port = 14404
 

startServer :: IO ()
startServer = withSocketsDo $ do
  sock <- listenOn $ PortNumber port
  Prelude.putStrLn $ "Listening on port " ++ show port ++ "."
  _ <- loop sock
  sClose sock
 
loop :: Socket -> IO a
loop sock = do
  (conn, _) <- accept sock
  _ <- forkIO $ talk conn
  loop sock

talk :: Socket -> IO ()
talk c = do
  rawRequest <- recv c bufferSize
  let cmd = parseRawRequest rawRequest
  let res = buildResponse cmd
  sendAll c res
  -- close connection if invalid command,
  -- otherwise continue talking
  if (cmd == CmdUnknown) then do sClose c 
                         else do talk c

