module Ircd.Utils
    ( addThreadIdToQuitMVar
    , delThreadIdFromQuitMVar
    , setGlobalQuitMVar
    ) where

import Control.Concurrent
import Control.Monad.Reader
import qualified Data.List as L

import Ircd.Types

-- | utility functions
addThreadIdToQuitMVar :: ThreadId -> Ircd IO ()
addThreadIdToQuitMVar thrId = do
    threadIdsMv <- asks envThreadIdsMv
    liftIO $ modifyMVar_ threadIdsMv (\l -> return $ thrId:l)

delThreadIdFromQuitMVar :: ThreadId -> Ircd IO ()
delThreadIdFromQuitMVar thrId = do
    threadIdsMv <- asks envThreadIdsMv
    liftIO $ modifyMVar_ threadIdsMv (\l -> return $ L.delete thrId l)

setGlobalQuitMVar :: IrcdStatus -> Ircd IO ()
setGlobalQuitMVar status = do
    quitMv <- asks envQuitMv
    liftIO $ putMVar quitMv status

