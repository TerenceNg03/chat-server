{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Network.Socket (Family (AF_INET), PortNumber, SockAddr (SockAddrInet), SocketType (Stream), bind, listen, socket, accept, socketToHandle)
import System.IO (BufferMode (LineBuffering), Handle, hGetLine, hPrint, hPutStrLn, hSetBuffering, IOMode (ReadWriteMode), hClose)
import Fmt (format)
import Control.Monad (forever)
import Control.Concurrent (forkFinally)

talk :: Handle -> IO ()
talk h = do
    hSetBuffering h LineBuffering
    loop
  where
    loop = do
        line <- hGetLine h
        if line == "end"
            then
                hPutStrLn
                    h
                    ( "Thank you for using the "
                        ++ "Haskell doubling service."
                    )
            else do
                hPrint h (2 * (read line :: Integer))
                loop

main :: IO ()
main = do
    sock <- socket AF_INET Stream 0
    bind sock (SockAddrInet port 0)
    listen sock 5
    putStrLn $ format "Listening to port {}" (show port)
    forever $ do
        (sock', addr) <- accept sock
        putStrLn $ format "Accepted connection : {}" (show addr)
        handle <- socketToHandle sock' ReadWriteMode 
        forkFinally (talk handle) (const $ hClose handle)

port :: PortNumber
port = 44444
