module Ircd
    ( ircd
    ) where

import qualified Config.Dyre as Dyre
import Config.Dyre.Relaunch
import System.IO

import Ircd.Config

startIrcd :: Config -> IO ()
startIrcd config = do
    case configErrors config of
         Nothing -> return ()
         Just em -> putStrLn $ "Error: " ++ em
    print config
    putStr "> " >> hFlush stdout
    input <- getLine
    case input of
         "exit" -> return ()
         "quit" -> return ()
         _      -> relaunchMaster Nothing

ircd :: Config -> IO ()
ircd = Dyre.wrapMain $ Dyre.defaultParams
    { Dyre.projectName = "hsircd"
    , Dyre.realMain    = startIrcd
    , Dyre.showError   = (\config err -> config { configErrors = Just err })
    }

