{-# LANGUAGE ScopedTypeVariables #-}
module Ircd.Peer
    ( defaultPeerState
    , handlePeerRequests
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

import Ircd.Command
import Ircd.Types
import Ircd.Utils

defaultPeerState :: PeerState
defaultPeerState = PeerState
    { peerStatus = UNREGISTERED
    , peerPass   = Nothing
    , peerNick   = Nothing
    , peerUser   = Nothing }

handlePeerRequests :: PeerEnv -> Env IO ()
handlePeerRequests peerEnv = do
    env <- ask
    let connhdl = peerHandle peerEnv
        chan    = peerChan peerEnv
        addr    = peerClientAddr peerEnv
        ctx     = peerTLSCtx peerEnv
    myOwnThreadId  <- liftIO myThreadId
    -- We spawn the socket's reader
    readerThreadId <- liftIO . forkIO $ peerReader connhdl ctx chan myOwnThreadId
    -- Then we run the main peer loop
    status <- liftIO $ runReaderT (runReaderT peerCore peerEnv) env
                `catches` [ Handler (\ (_ :: IOException) -> return Exit)
                          , Handler (\ (_ :: AsyncException) -> return Exit) ]
    -- Finally we terminate properly
    liftIO $ killThread readerThreadId
    case ctx of
        Just sCtx -> bye sCtx
        Nothing   -> return ()
    liftIO $ hClose connhdl -- TODO : have a way to check if we are reloading or not
    liftIO myThreadId >>= delThreadIdFromQuitMVar
    liftIO $ infoM "Ircd.peer" $ "peer disconnected " ++ show addr ++ " with status " ++ show status
    return ()

-- | Runs the IrcBot's reader loop
peerReader :: Handle -> Maybe (TLSCtx Handle) -> Chan Message -> ThreadId -> IO ()
peerReader _ (Just ctx) chan _ = forever $
    fmap L.toChunks (recvData ctx) >>= mapM_ (handleIncomingStr chan . C.unpack)  -- TODO exceptions
peerReader handle Nothing chan fatherThreadId = forever $
    hGetLine handle `catch` handleIOException >>= handleIncomingStr chan
  where
    handleIOException :: IOException -> IO String
    handleIOException ioException = do
        throwTo fatherThreadId ioException
        myId <- myThreadId
        killThread myId
        return ""

handleIncomingStr :: Chan Message -> String -> IO ()
handleIncomingStr chan str = do
    liftIO $ debugM "Ircd.peer" $ "<-- " ++ str
    case IRC.decode str of
        Just msg -> writeChan chan $ IncomingMsg msg
        Nothing -> return () -- TODO spam control

peerCore :: PEnv (Env IO) Status
peerCore = forever $ do
    msg <- asks peerChan >>= lift . liftIO . readChan
    case msg of
        IncomingMsg msg' -> processPeerCommand msg'
        OutgoingMsg msg' -> do
            liftIO . debugM "Ircd.peer" $ "--> " ++ show msg'
            handle <- asks peerHandle
            ctx <- asks peerTLSCtx
            lift . liftIO $ sendStr handle ctx $ IRC.encode msg' -- TODO exceptions
            return ()
        ClientMsg _ _ -> return ()
    return Continue

