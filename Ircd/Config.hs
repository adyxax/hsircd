module Ircd.Config
    ( Config (..)
    , Listen (..)
    , defaultConfig
    ) where

data Config = Config
    { configListen :: [Listen]
    , configErrors :: Maybe String
    } deriving (Read, Show)

data Listen = Listen
    { listenAddress :: String
    , listenPort    :: String
    } deriving (Read, Show)

defaultConfig :: Config
defaultConfig = Config { configListen = [ (Listen "127.0.0.1" "6667") ]
                       , configErrors = Nothing }

