module Ircd.Config
    ( Config (..)
    , defaultConfig
    ) where

data Config = Config
    { configListen :: [(String, String)]
    , configErrors :: Maybe String
    } deriving (Read, Show)

defaultConfig :: Config
defaultConfig = Config { configListen = [("127.0.0.1", "6667")]
                       , configErrors = Nothing }

