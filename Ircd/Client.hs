{-# OPTIONS_GHC -XScopedTypeVariables #-}
module Ircd.Client
    ( handleClientRequests
    ) where

import Control.Concurrent
import Control.Exception (AsyncException, Handler (..), IOException, catch, catches)
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import qualified Network.IRC as IRC
import Network.TLS
import Prelude hiding (catch)
import System.IO
import System.Log.Logger

import Ircd.Types
import Ircd.Utils

handleClientRequests :: ClientEnv -> Ircd IO ()
handleClientRequests clientEnv = do
    env <- ask
    let connhdl = clientHandle clientEnv
        chan    = clientChan clientEnv
        addr    = clientAddr clientEnv
        ctx     = clientTLSCtx clientEnv
    myOwnThreadId  <- liftIO $ myThreadId
    -- We spawn the socket's reader
    readerThreadId <- liftIO . forkIO $ clientReader connhdl ctx chan myOwnThreadId
    -- Then we run the main client loop
    status <- liftIO $ (runReaderT (runReaderT clientCore clientEnv) env)
                `catches` [ Handler (\ (_ :: IOException) -> return IrcdExit)
                          , Handler (\ (_ :: AsyncException) -> return IrcdExit) ]
    -- Finally we terminate properly
    liftIO $ killThread readerThreadId
    case ctx of
        Just sCtx -> bye sCtx
        Nothing   -> return ()
    liftIO $ hClose connhdl -- TODO : have a way to check if we are reloading or not
    (liftIO $ myThreadId) >>= delThreadIdFromQuitMVar
    liftIO $ infoM "Ircd.Client" $ "Client disconnected " ++ (show addr) ++ " with status " ++ (show status)
    return ()

-- | Runs the IrcBot's reader loop
clientReader :: Handle -> Maybe TLSCtx -> Chan Message -> ThreadId -> IO ()
clientReader _ (Just ctx) chan _ = forever $
    recvData ctx >>= return . L.toChunks >>= mapM_ (handleIncomingStr chan . C.unpack)  -- TODO exceptions
clientReader handle Nothing chan fatherThreadId = forever $
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

clientCore :: ReaderT ClientEnv (Ircd IO) (IrcdStatus)
clientCore = do
    chan <- asks clientChan
    msg <- lift . liftIO $ readChan chan
    -- For now we simply send the make the exchanges between the client and the core
    case msg of
        IrcMsg msg' -> do
            masterChan <- lift $ asks envChan
            cltState <- ask
            lift . liftIO $ writeChan masterChan $ ClientMsg cltState msg'
        OutgoingMsg msg' -> do
            liftIO $ debugM "Ircd.Client" $ "--> " ++ (show msg')
            handle <- asks clientHandle
            ctx <- asks clientTLSCtx
            lift . liftIO $ handleOutgoingStr handle ctx $ IRC.encode msg' -- TODO exceptions
            return ()
        ClientMsg _ _ -> return ()
    return IrcdContinue

handleOutgoingStr :: Handle -> Maybe TLSCtx -> String -> IO ()
handleOutgoingStr _ (Just ctx) msg = sendData ctx $ L.fromChunks [C.pack msg]
handleOutgoingStr handle Nothing msg = hPutStrLn handle msg

