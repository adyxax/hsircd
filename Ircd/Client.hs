{-# OPTIONS_GHC -XScopedTypeVariables #-}
module Ircd.Client
    ( handleClientRequests
    ) where

import Control.Concurrent
import Control.Exception (AsyncException, Handler (..), IOException, catch, catches)
import Control.Monad.Reader
import Control.Monad.State
import qualified Network.IRC as IRC
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
  where
    clientLoop :: ClientState -> Ircd IO (IrcdStatus, ClientState)
    clientLoop cltState = do
        env <- ask
        (status, cltState') <- liftIO $ (runReaderT (runStateT clientCore cltState) env)
                                      `catches` [ Handler (\ (_ :: IOException) -> return (IrcdExit, cltState))
                                                , Handler (\ (_ :: AsyncException) -> return (IrcdExit, cltState)) ]
        case status of
            IrcdContinue -> clientLoop cltState'
            _            -> return (status, cltState')

-- | Runs the IrcBot's reader loop
clientReader :: Handle -> Chan Message -> ThreadId -> IO ()
clientReader handle chan fatherThreadId = forever $ do
    str <- (hGetLine handle) `catch` handleIOException
    liftIO $ debugM "Ircd.Client" $ "<-- " ++ str
    -- TODO : check if it's a valid IRC message
    case IRC.decode str of
        Just msg -> writeChan chan $ IrcMsg msg
        Nothing -> return () -- TODO spam control
  where
    handleIOException :: IOException -> IO (String)
    handleIOException ioException = do
        throwTo fatherThreadId ioException
        myId <- myThreadId
        killThread myId
        return ""

clientCore :: Client (Ircd IO) (IrcdStatus)
clientCore = do
    chan <- gets clientChan
    msg <- lift . liftIO $ readChan chan
    -- For now we simply send the make the exchanges between the client and the core
    case msg of
        IrcMsg msg' -> do
            masterChan <- lift $ asks envChan
            cltState <- get
            lift . liftIO $ writeChan masterChan $ ClientMsg cltState msg'
        ServerMsg msg' -> do
            liftIO $ debugM "Ircd.Client" $ "--> " ++ (show msg')
            handle <- gets clientHandle
            lift . liftIO $ hPutStrLn handle $ IRC.encode msg'
        ClientMsg _ _ -> return ()
    return IrcdContinue

