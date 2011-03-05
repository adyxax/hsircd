module Ircd.Types
    ( Client
    , ClientState (..)
    , Ircd
    , IrcdEnv (..)
    , IrcdStatus (..)
    , Message (..)
    ) where

import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.State
import Network.Socket
import System.IO

data IrcdStatus = IrcdContinue | IrcdExit | IrcdReload | IrcdRestart deriving (Show)

type Ircd = ReaderT IrcdEnv

data IrcdEnv = IrcdEnv
    { envSockets     :: [Socket]
    , envChan        :: Chan Message
    , envQuitMv      :: MVar (IrcdStatus)
    , envThreadIdsMv :: MVar [ThreadId]
    }

type Client = StateT ClientState

data ClientState = ClientState
    { clientHandle :: Handle
    , clientChan   :: Chan Message
    , clientSocket :: Socket
    , clientAddr   :: SockAddr
    }

data Message = Message
    { msgSender  :: Maybe ClientState
    , msgContent :: String
    }

