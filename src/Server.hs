{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}

module Server (newServer, talk) where

import Client (Client (sendChan) , ClientStatus (Login, Logout, Kicked), newClient, sendMessage)
import Control.Concurrent.Async (race)
import Control.Concurrent.STM (STM, TVar, atomically, modifyTVar, newTVarIO, readTChan, readTVar, writeTVar, TChan, newBroadcastTChanIO, dupTChan, writeTChan)
import Control.Exception (bracket)
import Control.Monad (forever, join, void, when)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (IsString (fromString))
import Data.Text (strip, stripPrefix, unpack)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Fmt (format)
import Message (ClientName, Message (BroadcastTo, Login, Notice, Logout, Broadcast))
import Optics (makeFieldLabelsNoPrefix, (^.))
import System.IO (BufferMode (LineBuffering), Handle, hGetLine, hPutStrLn, hSetBuffering, hSetNewlineMode, universalNewlineMode)

data Server = Server
    { clients :: TVar (Map UUID Client)
    , names :: TVar (Set ClientName)
    , broadcastChan :: TChan Message
    }

makeFieldLabelsNoPrefix ''Server

newServer :: IO Server
newServer = do
    cs <- newTVarIO Map.empty
    n <- newTVarIO Set.empty
    c <- newBroadcastTChanIO
    return Server{clients = cs, names = n, broadcastChan = c}

broadcast :: Server -> Message -> STM ()
broadcast Server{..} = writeTChan broadcastChan

parseMsg :: String -> Maybe Message
parseMsg s = case fromString s of
    (stripPrefix "/login" -> Just name) -> Just . Message.Login $ strip name
    (stripPrefix "/" -> Just _) -> Nothing
    _ -> Just $ BroadcastTo (fromString s)

runClient :: Server -> Client -> IO ()
runClient server client = do
    void $ race receive (race process forward)
  where
    h = client ^. #handle
    receive = forever $ do
        line <- hGetLine h
        let msg = parseMsg line
        case msg of
            Just msg' -> atomically $ sendMessage client msg'
            Nothing -> do
                hPutStrLn h "Invalid command!"

    forward = forever $ atomically $ do
        msg <- readTChan $ client ^. #recvChan
        writeTChan (client ^. #sendChan) msg

    process = join $ atomically $ do
        msg <- readTChan $ client ^. #sendChan
        case msg of
            Notice s -> return $ do
                hPutStrLn h $ "Notice :\n" <> unpack s
                process
            Message.Login name -> do
                status <- readTVar $ client ^. #status
                case status of
                    Client.Login name' -> return $ do
                        hPutStrLn h $ format "Already logged in with name \"{}\"" name'
                        process
                    _ -> do
                        names <- readTVar $ server ^. #names
                        if Set.member name names
                            then return $ do
                                hPutStrLn h $ format "Name \"{}\" is already taken!" name
                                process
                            else do
                                writeTVar (client ^. #status) $ Client.Login name
                                modifyTVar (server ^. #names) $ Set.insert name
                                broadcast server $ Notice $ format "{} has logged in!" name
                                return process
            Message.Logout -> do
                status <- readTVar $ client ^. #status
                case status of
                    Client.Login name -> do
                        writeTVar (client ^. #status) Client.Logout
                        modifyTVar (server ^. #names) $ Set.delete name
                        broadcast server $ Notice $ format "{} has logged out!" name
                        return process
                    _ -> return process
            BroadcastTo msg' -> do
                status <- readTVar $ client ^. #status
                case status of
                    Client.Login name -> do
                        broadcast server $ Broadcast name msg'
                        return process
                    Kicked reason -> return $ do
                        hPutStrLn h $ format "You are kicked due to {}!" reason
                        process
                    Client.Logout -> return $ do
                        hPutStrLn h "You are not logged in!"
                        process
            Broadcast name msg' -> do
                status <- readTVar $ client ^. #status
                let ok = case status of
                        Client.Login name' -> name /= name'
                        _ -> True
                return $ do
                    when ok . hPutStrLn h $ format "Message form {} :\n{}" name msg'
                    process
            _ -> error "Not supported yet"

talk :: Handle -> Server -> IO ()
talk handle server = do
    hSetNewlineMode handle universalNewlineMode
    hSetBuffering handle LineBuffering
    uuid <- nextRandom
    let getClient = atomically $ do
            recvChan <- dupTChan $ server ^. #broadcastChan
            newClient uuid recvChan handle
        removeClient client = atomically $ do
            modifyTVar (server ^. #clients) $ Map.delete uuid
            status <- readTVar $ client ^. #status
            case status of
                Client.Login name -> do 
                    modifyTVar (server ^. #names) $ Set.delete name
                    broadcast server $ Notice $ format "{} has logged out!" name
                _ -> return ()
    bracket getClient removeClient $ runClient server
