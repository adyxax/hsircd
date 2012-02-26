module Ircd.Core
    ( initIrcd
    , runIrcd
    , terminateIrcd
    ) where

import Control.Concurrent
import Control.Monad.Reader
import Crypto.Random
import qualified Data.Map as M
import Network.Socket
import Network.TLS
import System.IO
import System.Log.Logger

import Ircd.Peer
import Ircd.Types
import Ircd.Utils

defaultIrcdState :: IrcdState
defaultIrcdState = IrcdState
    { ircdPeers = []
    , ircdNicks = M.empty
    , ircdNicksHistory = M.empty
    , ircdChans = M.empty }

initIrcd :: Config -> IO IrcdEnv
initIrcd config = do
    ircdState <- newMVar defaultIrcdState
    liftIO $ infoM "Ircd.Core" "Binding server"
    mySockets <- mapM initSocket $ configListen config
    let tlsConfig = configTLS config
    tls <- if tlsOn tlsConfig
            then (do
                liftIO $ infoM "Ircd.Core" "TLS init"
                fmap Just (initTLSEnv tlsConfig))
            else return Nothing
    chan <- newChan :: IO (Chan Message)
    threadIdsMv <- newMVar []
    quitMv <- newEmptyMVar
    return IrcdEnv { envIrcdState   = ircdState
                   , envSockets     = mySockets
                   , envChan        = chan
                   , envQuitMv      = quitMv
                   , envThreadIdsMv = threadIdsMv
                   , envTLS         = tls
                   , envConfig      = config }

runIrcd :: Env IO Status
runIrcd = do
    -- First we spawn all accept threads
    asks envSockets >>= mapM_ forkAccept
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
        peerstate <- liftIO $ newMVar defaultPeerState
        env <- ask
        (connsock, clientaddr) <- liftIO $ accept masterSocket
        liftIO . infoM "Ircd.Peer" $ "Receiving incoming connection " ++ show clientaddr
        connhdl <- liftIO $ socketToHandle connsock ReadWriteMode
        liftIO $ hSetBuffering connhdl NoBuffering
        liftIO $ hSetEncoding connhdl utf8
        ctx <- case envTLS env of
            Just params -> do
                randomGen <- liftIO (newGenIO :: IO SystemRandom)
                ctx <- server params randomGen connhdl
                handshake ctx
                return $ Just ctx
            Nothing  -> return Nothing
        let thePeer = PeerEnv { peerState  = peerstate
                              , peerHandle = connhdl
                              , peerSocket = connsock
                              , peerTLSCtx = ctx
                              , peerClientAddr = clientaddr }
        liftIO (forkIO $ runReaderT (handlePeerRequests thePeer) env) >>= addThreadIdToQuitMVar

terminateIrcd :: Env IO ()
terminateIrcd = do
    liftIO $ infoM "Ircd.Core" "Closing sockets"
    sockets <- asks envSockets
    liftIO $ mapM_ (`shutdown` ShutdownBoth) sockets
    liftIO $ mapM_ sClose sockets

