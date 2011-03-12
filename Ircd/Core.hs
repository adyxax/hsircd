module Ircd.Core
    ( initIrcd
    , runIrcd
    , terminateIrcd
    ) where

import Control.Concurrent
import Control.Monad.Reader
import Network.Socket
import Network.TLS
import System.IO
import System.Log.Logger

import Ircd.Client
import Ircd.Config
import Ircd.Types
import Ircd.Utils

initIrcd :: Config -> IO (IrcdEnv)
initIrcd config = do
    mySockets <- mapM initSocket $ configListen config
    tls <- case sslOn $ configSSL config of
        True  -> initTLSEnv >>= return . Just
        False -> return Nothing
    chan <- newChan :: IO (Chan Message)
    threadIdsMv <- newMVar []
    quitMv <- newEmptyMVar
    return IrcdEnv { envSockets     = mySockets
                   , envChan        = chan
                   , envQuitMv      = quitMv
                   , envThreadIdsMv = threadIdsMv
                   , envTLS         = tls }
  where
    initSocket :: Listen -> IO (Socket)
    initSocket (Listen hostname port) = do
        liftIO $ infoM "Ircd.Core" $ "Listening on " ++ hostname ++ ":" ++ port
        addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
        let serveraddr = head addrinfos
        mySocket <- socket (addrFamily serveraddr) Stream defaultProtocol
        setSocketOption mySocket ReuseAddr 1
        bindSocket mySocket (addrAddress serveraddr)
        listen mySocket 10
        return mySocket
    initTLSEnv :: IO (TLSParams)
    initTLSEnv = do
        let ssl = configSSL config
            certFile = sslCert ssl
            keyFile  = sslKey ssl
            versions = sslVersions ssl
            ciphers  = sslCiphers ssl
            verify   = sslVerify ssl
        cert <- readCertificate certFile
        pk   <- readPrivateKey keyFile
        return $ defaultParams { pConnectVersion = TLS12
                               , pAllowedVersions = versions
                               , pCiphers = ciphers
                               , pWantClientCert = verify
                               , pCertificates = [(cert, Just pk)] }

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
        -- Are we using
        ctx <- case envTLS env of
            Just params -> do
                randomGen <- liftIO $ makeSRandomGen >>= either (fail . show) (return . id)
                sCtx <- server params randomGen connhdl
                handshake sCtx
                return $ Just sCtx
            Nothing  -> return Nothing
        let theClient = ClientState { clientHandle = connhdl
                                    , clientChan   = chan
                                    , clientSocket = connsock
                                    , clientAddr   = clientaddr
                                    , clientTLSCtx = ctx }
        liftIO (forkIO $ runReaderT (handleClientRequests theClient) env) >>= addThreadIdToQuitMVar

    ircdCore :: Ircd IO ()
    ircdCore = do
        chan <- asks envChan
        msg <- liftIO $ readChan chan
        case msg of
            IrcMsg _ -> return ()
            ServerMsg _ -> return ()
            ClientMsg _ _ -> return ()

terminateIrcd :: Ircd IO ()
terminateIrcd = do
    liftIO $ infoM "Ircd.Core" "Closing sockets"
    sockets <- asks envSockets
    liftIO $ mapM_ (flip shutdown ShutdownBoth) sockets
    liftIO $ mapM_ sClose sockets

