module Ircd
    ( ircd
    ) where

import qualified Config.Dyre as Dyre
import Config.Dyre.Relaunch
import Control.Monad.State

import Ircd.Config
import Ircd.Core

startIrcd :: Config -> IO ()
startIrcd config = do
    -- checking for configuration file compilation error
    case configErrors config of
         Nothing -> return ()
         Just em -> putStrLn $ "Error: " ++ em
    -- initialization
    ircdEnv <- initIrcd config
    -- main stuff
    (status, _) <- runStateT runIrcd ircdEnv
    -- closing sockets
    evalStateT terminateIrcd ircdEnv
    -- Handling exit signal
    case status of
         IrcdExit -> return ()
         IrcdReload -> relaunchMaster Nothing -- TODO relaunchWithTextState (state { stateConfig = config }) Nothing
         IrcdRestart -> relaunchMaster Nothing

ircd :: Config -> IO ()
ircd = Dyre.wrapMain $ Dyre.defaultParams
    { Dyre.projectName = "hsircd"
    , Dyre.realMain    = startIrcd
    , Dyre.showError   = (\config err -> config { configErrors = Just err })
    }

