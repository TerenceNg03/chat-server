{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Concurrent (forkFinally)
import Control.Concurrent.Async (race)
import Control.Concurrent.STM (TChan, TVar, atomically, newTChanIO, newTVarIO, readTChan, readTVar, readTVarIO, writeTChan, writeTVar)
import Control.Monad (forever, join, void)
import Fmt (format)
import Network.Socket (Family (AF_INET), PortNumber, SockAddr (SockAddrInet), SocketType (Stream), accept, bind, listen, socket, socketToHandle)
import System.IO (BufferMode (LineBuffering), Handle, IOMode (ReadWriteMode), hClose, hGetLine, hPrint, hPutStrLn, hSetBuffering)

receiver :: Handle -> TChan String -> IO ()
receiver h c = forever $ do
    line <- hGetLine h
    atomically $ writeTChan c line

server :: Handle -> TVar Int -> TChan String -> IO ()
server h v c = do
    f <- readTVarIO v
    hPutStrLn h $ format "Current factor : {}" f
    loop f
  where
    loop f =
        join $ atomically $ do
            f' <- readTVar v
            if f /= f'
                then return (newFactor f')
                else do
                    l <- readTChan c
                    return (response f l)
    newFactor f = do
        hPutStrLn h $ format "New factor : {}" f
        loop f
    response f line = case line of
        "end" -> hPutStrLn h "Thank you for using the Haskell doubling service."
        '*' : s -> do
            atomically $ writeTVar v (read s)
            loop f
        _ -> do
            hPrint h (f * read @Int line)
            loop f

talk :: Handle -> TVar Int -> IO ()
talk h v = do
    hSetBuffering h LineBuffering
    c <- newTChanIO
    void $ race (server h v c) (receiver h c)

main :: IO ()
main = do
    sock <- socket AF_INET Stream 0
    bind sock (SockAddrInet port 0)
    listen sock 5
    putStrLn $ format "Listening to port {}" (show port)
    factor <- newTVarIO 2
    forever $ do
        (sock', addr) <- accept sock
        putStrLn $ format "Accepted connection : {}" (show addr)
        handle <- socketToHandle sock' ReadWriteMode
        forkFinally (talk handle factor) (const $ hClose handle)

port :: PortNumber
port = 44444
