module Ircd.Types
    ( Env
    , IrcdEnv (..)
    , IrcdState (..)
    , Message (..)
    , PEnv
    , PeerEnv (..)
    , PeerState (..)
    , PeerStatus (..)
    , Status (..)
    ) where

import Control.Concurrent
import Control.Monad.Reader
import qualified Network.IRC as IRC
import Network.Socket
import Network.TLS
import System.IO

-- Status
data Status = Continue | Exit | Restart deriving (Show)

-- Server environment
type Env = ReaderT IrcdEnv

data IrcdEnv = IrcdEnv
    { envIrcdState   :: MVar IrcdState
    , envSockets     :: [Socket]
    , envChan        :: Chan Message
    , envQuitMv      :: MVar Status
    , envThreadIdsMv :: MVar [ThreadId]
    , envTLS         :: Maybe TLSParams
    }

-- The Peer environment
type PEnv = ReaderT PeerEnv

data PeerEnv = PeerEnv
    { peerState       :: MVar PeerState
    , peerHandle      :: Handle
    , peerSocket      :: Socket
    , peerChan        :: Chan Message
    , peerQuitMv      :: MVar Status
    , peerThreadIdsMv :: MVar [ThreadId]
    , peerTLSCtx      :: Maybe (TLSCtx Handle)
    , peerClientAddr  :: SockAddr
    }

-- Server State
data IrcdState = IrcdState
    { ircdClients :: [PeerState]
    }

-- Peer state
data PeerState = PeerState
    { peerStatus :: PeerStatus
    , peerPass   :: Maybe String
    , peerNick   :: Maybe String
    , peerUser   :: Maybe String
    }

data PeerStatus = UNREGISTERED | REGISTERING | REGISTERED

data Message = ClientMsg PeerEnv IRC.Message
             | IncomingMsg IRC.Message
             | OutgoingMsg IRC.Message

