{-# OPTIONS_GHC -XScopedTypeVariables #-}
module Ircd.Client
    ( handleClientRequests
    ) where

import Control.Concurrent
import Control.Exception (AsyncException, Handler (..), IOException, catch, catches)
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import qualified Network.IRC as IRC
import Network.TLS
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
        ctx     = clientTLSCtx clientState
    myOwnThreadId  <- liftIO $ myThreadId
    liftIO $ infoM "Ircd.Client" $ "Client connected " ++ (show addr)
    -- We spawn the socket's reader
    readerThreadId <- liftIO $ forkIO $ clientReader connhdl ctx chan myOwnThreadId
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
clientReader :: Handle -> Maybe TLSCtx -> Chan Message -> ThreadId -> IO ()
clientReader _ (Just ctx) chan _ = forever $ do
    buff <- recvData ctx
    mapM_ (handleIncomingStr chan . C.unpack) (L.toChunks buff) -- TODO exceptions
clientReader handle Nothing chan fatherThreadId = forever $ do
    (hGetLine handle) `catch` handleIOException >>= handleIncomingStr chan
  where
    handleIOException :: IOException -> IO (String)
    handleIOException ioException = do
        throwTo fatherThreadId ioException
        myId <- myThreadId
        killThread myId
        return ""

handleIncomingStr :: Chan Message -> String -> IO ()
handleIncomingStr chan str = do
    liftIO $ debugM "Ircd.Client" $ "<-- " ++ str
    case IRC.decode str of
        Just msg -> writeChan chan $ IrcMsg msg
        Nothing -> return () -- TODO spam control

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
            ctx <- gets clientTLSCtx
            lift . liftIO $ handleOutgoingStr handle ctx $ IRC.encode msg' -- TODO exceptions
            return ()
        ClientMsg _ _ -> return ()
    return IrcdContinue

handleOutgoingStr :: Handle -> Maybe TLSCtx -> String -> IO ()
handleOutgoingStr _ (Just ctx) msg = sendData ctx $ L.fromChunks [C.pack msg]
handleOutgoingStr handle Nothing msg = hPutStrLn handle msg

