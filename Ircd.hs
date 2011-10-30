module Ircd
    ( ircd
    ) where

import qualified Config.Dyre as Dyre
import Config.Dyre.Relaunch
import Control.Monad.Reader
import System.Log.Logger

import Ircd.Config
import Ircd.Core
import Ircd.Types

startIrcd :: Config -> IO ()
startIrcd config = do
    -- checking for configuration file compilation error
    case configErrors config of
         Nothing -> return ()
         Just em -> putStrLn $ "Error: " ++ em
    -- initialization
    ircdEnv <- initIrcd config
    -- main stuff
    infoM "Ircd" "Server starting"
    status <- runReaderT runIrcd ircdEnv
    infoM "Ircd" $ "Server exited with status " ++ show status
    -- Handling exit signal
    case status of
         Continue -> startIrcd config -- TODO do something not so dumb about starting over
         Exit -> runReaderT terminateIrcd ircdEnv
         Restart -> relaunchMaster Nothing -- TODO relaunch and kill sockets

ircd :: Config -> IO ()
ircd = Dyre.wrapMain $ Dyre.defaultParams
    { Dyre.projectName = "hsircd"
    , Dyre.realMain    = startIrcd
    , Dyre.showError   = \config err -> config { configErrors = Just err } }

