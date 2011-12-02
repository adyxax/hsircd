module Ircd.Types
    ( Config (..)
    , Env
    , IrcdEnv (..)
    , IrcdState (..)
    , Listen (..)
    , Message (..)
    , PEnv
    , PeerEnv (..)
    , PeerState (..)
    , PeerStatus (..)
    , Status (..)
    , TLSConfig (..)
    ) where

import Control.Concurrent
import qualified Data.Map as M
import Control.Monad.Reader
import qualified Network.IRC as IRC
import Network.Socket
import Network.TLS
import System.IO

-- Config data types
data Config = Config
    { configErrors     :: Maybe String
    , configListen     :: [Listen]
    , configServerName :: String
    , configTLS        :: TLSConfig
    } deriving (Show)

data Listen = Listen
    { listenAddress     :: String
    , listenPort        :: String
    } deriving (Show)

data TLSConfig = TLSConfig
    { tlsOn       :: Bool
    , tlsCert     :: String
    , tlsKey      :: String
    , tlsVerify   :: Bool
    } deriving (Show)

-- Status
data Status = Continue | Exit String | Restart String deriving (Show)

-- Server environment
type Env = ReaderT IrcdEnv

data IrcdEnv = IrcdEnv
    { envIrcdState   :: MVar IrcdState
    , envSockets     :: [Socket]
    , envChan        :: Chan Message
    , envQuitMv      :: MVar Status
    , envThreadIdsMv :: MVar [ThreadId]
    , envTLS         :: Maybe TLSParams
    , envConfig      :: Config
    }

-- The Peer environment
type PEnv = ReaderT PeerEnv

data PeerEnv = PeerEnv
    { peerState       :: MVar PeerState
    , peerHandle      :: Handle
    , peerSocket      :: Socket
    , peerTLSCtx      :: Maybe (TLSCtx Handle)
    , peerClientAddr  :: SockAddr
    }

instance Eq PeerEnv where
  a == b = peerHandle a == peerHandle b

instance Show PeerEnv where
  show = show . peerHandle

-- Server State
data IrcdState = IrcdState
    { ircdPeers :: [PeerEnv]
    , ircdNicks :: M.Map String PeerEnv
    , ircdChans :: M.Map String [String]
    } deriving (Show)

-- Peer state
data PeerState = PeerState
    { peerStatus   :: PeerStatus
    , peerPass     :: Maybe String
    , peerNick     :: Maybe String
    , peerUser     :: Maybe (IRC.UserName, HostName, IRC.ServerName, IRC.RealName)
    , peerChans    :: [String]
    , peerIsServer :: Bool
    }

data PeerStatus = UNREGISTERED | REGISTERING | REGISTERED deriving (Eq)

data Message = ClientMsg PeerEnv IRC.Message
             | IncomingMsg IRC.Message
             | OutgoingMsg IRC.Message

