module Ircd.Config
    ( defaultConfig
    , defaultListener
    , defaultTLSConfig
    , noTLS
    ) where

import Ircd.Types

defaultConfig :: Config
defaultConfig = Config
    { configListen     = [ defaultListener ]
    , configErrors     = Nothing
    , configServerName = "hsircd.example.com"
    , configTLS        = noTLS }

defaultListener :: Listen
defaultListener = Listen
    { listenAddress     = "0.0.0.0"
    , listenPort        = "6667" }

defaultTLSConfig :: TLSConfig
defaultTLSConfig = TLSConfig
    { tlsOn       = True
    , tlsCert     = ""
    , tlsKey      = ""
    , tlsVerify   = True }

noTLS :: TLSConfig
noTLS = defaultTLSConfig { tlsOn = False }

