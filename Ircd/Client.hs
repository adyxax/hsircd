{-# OPTIONS_GHC -XScopedTypeVariables #-}
module Ircd.Client
    ( handleClientRequests
    ) where

import Control.Concurrent
import Control.Exception (AsyncException, Handler (..), IOException, catch, catches)
import Control.Monad.Reader
import Control.Monad.State
import Prelude hiding (catch)
import System.IO
import System.Log.Logger

import Ircd.Types
import Ircd.Utils

handleClientRequests :: ClientState -> Ircd IO ()
handleClientRequests clientState = do
    let connhdl = clientHandle clientState
        chan    = clientChan clientState
        addr    = clientAddr clientState
    myOwnThreadId  <- liftIO $ myThreadId
    liftIO $ infoM "Ircd.Client" $ "Client connected " ++ (show addr)
    -- We spawn the socket's reader
    readerThreadId <- liftIO $ forkIO $ clientReader connhdl chan myOwnThreadId
    (status, _) <- clientLoop clientState
    -- Finally we terminate properly   TODO : have a way to check if we are reloading
    liftIO $ killThread readerThreadId
    liftIO $ hClose connhdl
    (liftIO $ myThreadId) >>= delThreadIdFromQuitMVar
    liftIO $ infoM "Ircd.Client" $ "Client disconnected " ++ (show addr) ++ " with status " ++ (show status)
    return ()

-- | Runs the IrcBot's reader loop
clientReader :: Handle -> Chan Message -> ThreadId -> IO ()
clientReader handle chan fatherThreadId = forever $ do
    str <- (hGetLine handle) `catch` handleIOException
    liftIO $ debugM "Ircd.Client" $ "<-- " ++ str
    -- TODO : check if it's a valid IRC message
    writeChan chan $ Message Nothing str
  where
    handleIOException :: IOException -> IO (String)
    handleIOException ioException = do
        throwTo fatherThreadId ioException
        myId <- myThreadId
        killThread myId
        return ""

clientLoop :: ClientState -> Ircd IO (IrcdStatus, ClientState)
clientLoop clientState = do
    env <- ask
    (status, clientState') <- liftIO $ (runReaderT (runStateT clientCore clientState) env)
                                  `catches` [ Handler (\ (_ :: IOException) -> return (IrcdExit, clientState))
                                            , Handler (\ (_ :: AsyncException) -> return (IrcdExit, clientState)) ]
    case status of
        IrcdContinue -> clientLoop clientState'
        _            -> return (status, clientState')

clientCore :: Client (Ircd IO) (IrcdStatus)
clientCore = do
    chan <- gets clientChan
    msg <- lift . liftIO $ readChan chan
    -- For now we simply send the make the exchanges between the client and the core
    case msgSender msg of
        Just sender -> do
            liftIO $ debugM "Ircd.Client" $ "--> " ++ (msgContent msg)
            handle <- gets clientHandle
            lift . liftIO $ hPutStrLn handle (msgContent msg)
        Nothing -> do
            masterChan <- lift $ asks envChan
            state <- get
            lift . liftIO $ writeChan masterChan $ Message (Just state) (msgContent msg)
    return IrcdContinue

