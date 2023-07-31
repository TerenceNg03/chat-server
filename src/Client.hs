{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module Client (Client (..), ClientStatus (..), newClient, sendMessage) where

import Control.Concurrent.STM (TChan, TVar, newTChan, newTVar, writeTChan)
import Control.Monad.STM (STM)
import Data.Text (Text)
import Data.UUID (UUID)
import Message (ClientName, Message)
import Optics (makeFieldLabelsNoPrefix, (^.))
import System.IO (Handle)

data ClientStatus = Login ClientName | Kicked Text | Logout
makeFieldLabelsNoPrefix ''ClientStatus

data Client = Client
    { uuid :: UUID
    , status :: TVar ClientStatus
    , handle :: Handle
    , sendChan :: TChan Message
    , recvChan :: TChan Message
    }

makeFieldLabelsNoPrefix ''Client

newClient :: UUID -> TChan Message -> Handle -> STM Client
newClient uuid c handle = do
    status <- newTVar Logout
    sendChan <- newTChan
    return
        Client
            { uuid = uuid
            , status = status
            , handle = handle
            , sendChan = sendChan
            , recvChan = c
            }

sendMessage :: Client -> Message -> STM ()
sendMessage client = writeTChan $ client ^. #sendChan
