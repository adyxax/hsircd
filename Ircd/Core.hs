module Ircd.Core
    ( initIrcd
    , runIrcd
    , terminateIrcd
    ) where

import Control.Concurrent
import Control.Monad.Reader
import Network.Socket
import System.IO

import Ircd.Client
import Ircd.Config
import Ircd.Types
import Ircd.Utils

initIrcd :: Config -> IO (IrcdEnv)
initIrcd config = do
    mySockets <- mapM initSocket $ configListen config
    chan <- newChan :: IO (Chan Message)
    threadIdsMv <- newMVar []
    quitMv <- newEmptyMVar
    return IrcdEnv { envSockets     = mySockets
                   , envChan        = chan
                   , envQuitMv      = quitMv
                   , envThreadIdsMv = threadIdsMv }
  where
    initSocket :: Listen -> IO (Socket)
    initSocket (Listen hostname port) = do
        addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
        let serveraddr = head addrinfos
        mySocket <- socket (addrFamily serveraddr) Stream defaultProtocol
        setSocketOption mySocket ReuseAddr 1
        bindSocket mySocket (addrAddress serveraddr)
        listen mySocket 10
        return mySocket

runIrcd :: Ircd IO (IrcdStatus)
runIrcd = do
    env <- ask
    -- First we spawn all accept threads
    asks envSockets >>= mapM_ (forkAccept)
    -- Then we make the server enter it's main loop
    liftIO (forkIO $ runReaderT (forever $ ircdCore) env) >>= addThreadIdToQuitMVar
    -- We wait for the quit signal
    quitMv <- asks envQuitMv
    code <- liftIO $ takeMVar quitMv
    -- and we clean things up
    threadIdsMv <- asks envThreadIdsMv
    liftIO (readMVar threadIdsMv) >>= liftIO . mapM_ killThread
    return code
  where
    forkAccept :: Socket -> Ircd IO ()
    forkAccept masterSocket = do
        env <- ask
        liftIO (forkIO $ runReaderT (forever $ ircdAccept masterSocket) env) >>= addThreadIdToQuitMVar

    ircdAccept :: Socket -> Ircd IO ()
    ircdAccept masterSocket = do
        env <- ask
        (connsock, clientaddr) <- liftIO $ accept masterSocket
        connhdl <- liftIO $ socketToHandle connsock ReadWriteMode
        liftIO $ hSetBuffering connhdl LineBuffering
        liftIO $ hSetEncoding connhdl utf8
        chan <- liftIO (newChan :: IO (Chan Message))
        let client = ClientState { clientHandle = connhdl
                                 , clientChan   = chan
                                 , clientSocket = connsock
                                 , clientAddr   = clientaddr }
        liftIO (forkIO $ runReaderT (handleClientRequests client) env) >>= addThreadIdToQuitMVar

    ircdCore :: Ircd IO ()
    ircdCore = do
        chan <- asks envChan
        msg <- liftIO $ readChan chan
        -- for now we just send back their messages to the clients unless it's quit
        case msgSender msg of
            Just sender ->
                case msgContent msg of
                    "quit" -> setGlobalQuitMVar IrcdExit
                    _      -> liftIO $ writeChan (clientChan sender) $ Message (Just sender) (msgContent msg)
            Nothing -> return ()

terminateIrcd :: Ircd IO ()
terminateIrcd = do
    sockets <- asks envSockets
    liftIO $ mapM_ (flip shutdown ShutdownBoth) sockets
    liftIO $ mapM_ sClose sockets

