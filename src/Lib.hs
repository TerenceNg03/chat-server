{-# LANGUAGE OverloadedStrings #-}

module Lib (runServer) where

import Control.Concurrent (forkFinally)
import Control.Monad (forever)
import Fmt (format)
import Network.Socket (Family (AF_INET), PortNumber, SockAddr (SockAddrInet), SocketType (Stream), accept, bind, listen, socket, socketToHandle)
import System.IO (IOMode (ReadWriteMode), hClose)
import Server (talk, newServer)

runServer :: PortNumber -> IO ()
runServer port = do
    sock <- socket AF_INET Stream 0
    bind sock (SockAddrInet port 0)
    listen sock 5
    putStrLn $ format "Listening to port {}" (show port)
    server <- newServer
    forever $ do
        (sock', addr) <- accept sock
        putStrLn $ format "Accepted connection : {}" (show addr)
        handle <- socketToHandle sock' ReadWriteMode
        forkFinally (talk handle server) (const $ hClose handle)
