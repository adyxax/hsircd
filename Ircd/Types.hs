module Ircd.Types
    ( ClientEnv (..)
    , Ircd
    , IrcdEnv (..)
    , IrcdStatus (..)
    , Message (..)
    ) where

import Control.Concurrent
import Control.Monad.Reader
import qualified Network.IRC as IRC
import Network.Socket
import Network.TLS
import System.IO

data IrcdStatus = IrcdContinue | IrcdExit | IrcdReload | IrcdRestart deriving (Show)

type Ircd = ReaderT IrcdEnv

data IrcdEnv = IrcdEnv
    { envSockets     :: [Socket]
    , envChan        :: Chan Message
    , envQuitMv      :: MVar (IrcdStatus)
    , envThreadIdsMv :: MVar [ThreadId]
    , envTLS         :: Maybe TLSParams
    }

data ClientEnv = ClientEnv
    { clientHandle :: Handle
    , clientChan   :: Chan Message
    , clientSocket :: Socket
    , clientAddr   :: SockAddr
    , clientTLSCtx :: Maybe TLSCtx
    }

data Message = ClientMsg ClientEnv IRC.Message
             | IrcMsg IRC.Message
             | OutgoingMsg IRC.Message

