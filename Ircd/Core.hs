module Ircd.Core
    ( initIrcd
    , runIrcd
    , terminateIrcd
    ) where

import Control.Concurrent
import Control.Monad.Reader
import Crypto.Random
import Network.Socket
import Network.TLS
import System.IO
import System.Log.Logger

import Ircd.Config
import Ircd.Peer
import Ircd.Types
import Ircd.Utils

initIrcd :: Config -> IO (IrcdEnv)
initIrcd config = do
    ircdState <- newEmptyMVar
    liftIO $ infoM "Ircd.Core" "Binding server"
    mySockets <- mapM initSocket $ configListen config
    let tlsConfig = configTLS config
    tls <- if tlsOn tlsConfig
            then (do
                liftIO $ infoM "Ircd.Core" "TLS init"
                initTLSEnv tlsConfig >>= return . Just)
            else (return Nothing)
    chan <- newChan :: IO (Chan Message)
    threadIdsMv <- newMVar []
    quitMv <- newEmptyMVar
    return IrcdEnv { envIrcdState = ircdState
                   , envSockets     = mySockets
                   , envChan        = chan
                   , envQuitMv      = quitMv
                   , envThreadIdsMv = threadIdsMv
                   , envTLS         = tls }

runIrcd :: Env IO (Status)
runIrcd = do
    -- First we spawn all accept threads
    asks envSockets >>= mapM_ (forkAccept)
    -- We wait for the quit signal
    code <- asks envQuitMv >>= liftIO . takeMVar
    -- and we clean things up
    asks envThreadIdsMv >>= liftIO . readMVar >>= liftIO . mapM_ killThread
    return code
  where
    forkAccept :: Socket -> Env IO ()
    forkAccept masterSocket = ask >>= liftIO . forkIO . runReaderT (forever $ ircdAccept masterSocket) >>= addThreadIdToQuitMVar

    ircdAccept :: Socket -> Env IO ()
    ircdAccept masterSocket = do
        peerstate <- liftIO $ newEmptyMVar
        env <- ask
        (connsock, clientaddr) <- liftIO $ accept masterSocket
        liftIO $ infoM "Ircd.Client" $ "Receiving incoming connection " ++ (show clientaddr)
        connhdl <- liftIO $ socketToHandle connsock ReadWriteMode
        liftIO $ hSetBuffering connhdl NoBuffering
        liftIO $ hSetEncoding connhdl utf8
        chan <- liftIO (newChan :: IO (Chan Message))
        ctx <- case envTLS env of
            Just params -> do
                randomGen <- liftIO (newGenIO :: IO SystemRandom)
                sCtx <- server params randomGen connhdl
                success <- handshake sCtx
                unless success . liftIO $ errorM "Hsbot.Core" "TLS handshake failed"  -- TODO: do some usefull error handling
                return $ Just sCtx
            Nothing  -> return Nothing
        quitmv <- liftIO $ newEmptyMVar
        threadidsmv <- liftIO $ newEmptyMVar
        let thePeer = PeerEnv { peerState  = peerstate
                              , peerHandle = connhdl
                              , peerSocket = connsock
                              , peerChan   = chan
                              , peerQuitMv = quitmv
                              , peerThreadIdsMv = threadidsmv
                              , peerTLSCtx = ctx
                              , peerClientAddr = clientaddr }
        liftIO (forkIO $ runReaderT (handlePeerRequests thePeer) env) >>= addThreadIdToQuitMVar

terminateIrcd :: Env IO ()
terminateIrcd = do
    liftIO $ infoM "Ircd.Core" "Closing sockets"
    sockets <- asks envSockets
    liftIO $ mapM_ (flip shutdown ShutdownBoth) sockets
    liftIO $ mapM_ sClose sockets

