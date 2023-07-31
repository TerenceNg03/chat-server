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

import Client (Client, ClientStatus (Kicked, Login, Logout), newClient, sendMessage)
import Control.Concurrent.Async (race)
import Control.Concurrent.STM (STM, TChan, TVar, atomically, dupTChan, modifyTVar, newBroadcastTChanIO, newTVarIO, readTChan, readTVar, writeTChan, writeTVar)
import Control.Exception (bracket)
import Control.Monad (forever, join, void, when)
import Data.Char (isSpace)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.String (IsString (fromString))
import Data.Text (strip, stripPrefix)
import qualified Data.Text as Text
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Fmt (format)
import Message (ClientName, Message (Broadcast, BroadcastTo, Kick, Login, Logout, Notice, Tell, TellTo, Disconnect))
import Optics (makeFieldLabelsNoPrefix, (^.))
import System.IO (BufferMode (LineBuffering), Handle, hGetLine, hPutStrLn, hSetBuffering, hSetNewlineMode, universalNewlineMode)
import Network.Socket (SockAddr)

data Server = Server
    { clients :: TVar (Map UUID Client)
    , names :: TVar (Map ClientName Client)
    , broadcastChan :: TChan Message
    }

makeFieldLabelsNoPrefix ''Server

newServer :: IO Server
newServer = do
    clients <- newTVarIO Map.empty
    names <- newTVarIO Map.empty
    broadcastChan <- newBroadcastTChanIO
    return Server{..}

broadcast :: Server -> Message -> STM ()
broadcast Server{..} = writeTChan broadcastChan

parseMsg :: String -> Maybe Message
parseMsg s = case fromString s of
    (stripPrefix "/login" -> Just name) -> Just . Message.Login $ strip name
    (stripPrefix "/logout" -> Just "") -> Just Message.Logout
    (stripPrefix "/disconnect" -> Just "") -> Just Message.Disconnect
    (stripPrefix "/kick" -> Just rest) ->
        let rest' = Text.dropWhile isSpace rest
            name = Text.takeWhile (not . isSpace) rest'
            message = Text.dropWhile isSpace . Text.dropWhile (not . isSpace) $ rest'
         in Just $ Kick name message
    (stripPrefix "/tell" -> Just rest) ->
        let rest' = Text.dropWhile isSpace rest
            name = Text.takeWhile (not . isSpace) rest'
            message = Text.dropWhile isSpace . Text.dropWhile (not . isSpace) $ rest'
         in Just $ TellTo name message
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

    onLogin action = do
        status <- readTVar $ client ^. #status
        case status of
            Client.Login name -> action name
            Kicked reason -> return $ do
                hPutStrLn h $ format "You are kicked due to {}!" reason
                process
            Client.Logout -> return $ do
                hPutStrLn h "You are not logged in!"
                process

    process = join $ atomically $ do
        msg <- readTChan $ client ^. #sendChan
        case msg of
            Notice s -> return $ do
                hPutStrLn h $ "Notice: " <> Text.unpack s
                process
            Message.Login name -> do
                status <- readTVar $ client ^. #status
                case status of
                    Client.Login name' -> return $ do
                        hPutStrLn h $ format "Already logged in with name \"{}\"" name'
                        process
                    _ -> do
                        names <- readTVar $ server ^. #names
                        if Map.member name names
                            then return $ do
                                hPutStrLn h $ format "Name \"{}\" is already taken!" name
                                process
                            else do
                                writeTVar (client ^. #status) $ Client.Login name
                                modifyTVar (server ^. #names) $ Map.insert name client
                                broadcast server $ Notice $ format "{} has logged in!" name
                                return process
            Message.Logout -> do
                status <- readTVar $ client ^. #status
                case status of
                    Client.Login name -> do
                        writeTVar (client ^. #status) Client.Logout
                        modifyTVar (server ^. #names) $ Map.delete name
                        broadcast server $ Notice $ format "{} has logged out!" name
                        return process
                    _ -> return process
            Disconnect -> return $ return ()
            BroadcastTo msg' -> onLogin $ \name -> do
                broadcast server $ Broadcast name msg'
                return process
            Broadcast name msg' -> do
                status <- readTVar $ client ^. #status
                let ok = case status of
                        Client.Login name' -> name /= name'
                        _ -> True
                return $ do
                    when ok . hPutStrLn h $ format "Broadcast form {}: {}" name msg'
                    process
            TellTo to msg' -> onLogin $ \from -> do
                clients <- readTVar $ server ^. #names
                case Map.lookup to clients of
                    Just target -> do
                        sendMessage target $ Tell from to msg'
                        return process
                    Nothing -> do
                        return $ do
                            hPutStrLn h $ format "User {} does not exists!" to
                            process
            Message.Tell from to msg' -> onLogin $ \name -> do
                if name == to then
                    return $ do
                        hPutStrLn h $ format "Tell from {}: {}" from msg'
                        process
                    else return process
            Kick to reason -> onLogin $ \from -> do
                clients <- readTVar $ server ^. #names
                case Map.lookup to clients of
                    Just target -> do
                        writeTVar (target ^. #status) $ Kicked reason
                        modifyTVar (server ^. #names) $ Map.delete to
                        broadcast server $ Notice $ format "{} is kicked by {} for {}" to from reason
                        return process
                    Nothing -> return $ do
                        hPutStrLn h $ format "User {} does not exists!" to
                        process

talk :: Handle -> Server -> SockAddr -> IO ()
talk handle server addr = do
    hSetNewlineMode handle universalNewlineMode
    hSetBuffering handle LineBuffering
    uuid <- nextRandom
    let getClient = atomically $ do
            recvChan <- dupTChan $ server ^. #broadcastChan
            newClient uuid recvChan handle
        removeClient client = join $ atomically $ do
            modifyTVar (server ^. #clients) $ Map.delete uuid
            status <- readTVar $ client ^. #status
            case status of
                Client.Login name -> do
                    modifyTVar (server ^. #names) $ Map.delete name
                    broadcast server $ Notice $ format "{} has logged out!" name
                _ -> return ()
            return $ putStrLn $ format "Disconnected: {}" (show addr)
    bracket getClient removeClient $ runClient server
