module Ircd.Core
    ( Ircd
    , IrcdState (..)
    , IrcdStatus (..)
    , initIrcd
    , runIrcd
    , terminateIrcd
    ) where

import Control.Monad.State
import Network.Socket

import Ircd.Config

data IrcdStatus = IrcdExit | IrcdReload | IrcdRestart

type Ircd = StateT IrcdState IO

data IrcdState = IrcdState
    { ircdSockets :: [Socket]
    } deriving (Show)

initIrcd :: Config -> IO (IrcdState)
initIrcd config = do
    mySockets <- mapM initSocket $ configListen config
    return IrcdState { ircdSockets = mySockets }
  where
    initSocket :: (String, String) -> IO (Socket)
    initSocket (hostname, port) = do
        addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
        let serveraddr = head addrinfos
        mySocket <- socket (addrFamily serveraddr) Stream defaultProtocol
        setSocketOption mySocket ReuseAddr 1
        bindSocket mySocket (addrAddress serveraddr)
        listen mySocket 10
        return mySocket

runIrcd :: Ircd (IrcdStatus)
runIrcd = do
    return IrcdExit

terminateIrcd :: Ircd ()
terminateIrcd = do
    sockets <- gets ircdSockets
    liftIO $ mapM_ (flip shutdown ShutdownBoth) sockets
    liftIO $ mapM_ sClose sockets

