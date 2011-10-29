module Ircd.Config
    ( Config (..)
    , Listen (..)
    , TLSConfig (..)
    , defaultConfig
    , defaultListener
    , defaultTLSConfig
    , noTLS
    ) where

data Config = Config
    { configListen :: [Listen]
    , configErrors :: Maybe String
    , configTLS    :: TLSConfig
    } deriving (Show)

defaultConfig :: Config
defaultConfig = Config
    { configListen = [ defaultListener ]
    , configErrors = Nothing
    , configTLS    = noTLS }

data Listen = Listen
    { listenAddress     :: String
    , listenPort        :: String
    } deriving (Show)

defaultListener :: Listen
defaultListener = Listen
    { listenAddress     = "0.0.0.0"
    , listenPort        = "6667" }

data TLSConfig = TLSConfig
    { tlsOn       :: Bool
    , tlsCert     :: String
    , tlsKey      :: String
    , tlsVerify   :: Bool
    } deriving (Show)

defaultTLSConfig :: TLSConfig
defaultTLSConfig = TLSConfig
    { tlsOn       = True
    , tlsCert     = ""
    , tlsKey      = ""
    , tlsVerify   = True }

noTLS :: TLSConfig
noTLS = defaultTLSConfig { tlsOn = False }

