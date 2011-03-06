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
import qualified Network.IRC as IRC
import Network.Socket
import Network.TLS.Server
import System.IO

data IrcdStatus = IrcdContinue | IrcdExit | IrcdReload | IrcdRestart deriving (Show)

type Ircd = ReaderT IrcdEnv

data IrcdEnv = IrcdEnv
    { envSockets     :: [Socket]
    , envChan        :: Chan Message
    , envQuitMv      :: MVar (IrcdStatus)
    , envThreadIdsMv :: MVar [ThreadId]
    , envTLS         :: Maybe TLSServerParams
    }

type Client = StateT ClientState

data ClientState = ClientState
    { clientHandle :: Handle
    , clientChan   :: Chan Message
    , clientSocket :: Socket
    , clientAddr   :: SockAddr
    }

data Message = ClientMsg ClientState IRC.Message
             | IrcMsg IRC.Message
             | ServerMsg IRC.Message

