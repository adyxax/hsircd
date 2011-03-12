module Ircd.Config
    ( Config (..)
    , Listen (..)
    , SSLParams (..)
    , defaultConfig
    , defaultListener
    , defaultSSLParams
    , noSSL
    ) where

import Network.TLS

data Config = Config
    { configListen :: [Listen]
    , configErrors :: Maybe String
    , configSSL    :: SSLParams
    } deriving (Show)

defaultConfig :: Config
defaultConfig = Config { configListen = [ defaultListener ]
                       , configErrors = Nothing
                       , configSSL    = noSSL }

data Listen = Listen
    { listenAddress     :: String
    , listenPort        :: String
    } deriving (Show)

defaultListener :: Listen
defaultListener = Listen
    { listenAddress     = "0.0.0.0"
    , listenPort        = "6667" }

data SSLParams = SSLParams
    { sslOn       :: Bool
    , sslCert     :: String
    , sslKey      :: String
    , sslVersions :: [Network.TLS.Version]
    , sslCiphers  :: [Network.TLS.Cipher]
    , sslVerify   :: Bool
    } deriving (Show)

defaultSSLParams :: SSLParams
defaultSSLParams = SSLParams
    { sslOn       = True
    , sslCert     = ""
    , sslKey      = ""
    , sslVersions = [SSL3, TLS10, TLS11, TLS12]
    , sslCiphers  = [ cipher_null_MD5
                    , cipher_null_SHA1
                    , cipher_AES128_SHA1
                    , cipher_AES256_SHA1
                    , cipher_RC4_128_MD5
                    , cipher_RC4_128_SHA1
                    , cipher_AES256_SHA1
                    , cipher_AES128_SHA256
                    , cipher_AES256_SHA256 ]
    , sslVerify   = True }

noSSL :: SSLParams
noSSL = defaultSSLParams { sslOn = False }

