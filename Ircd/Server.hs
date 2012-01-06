module Ircd.Server
    ( setNick
    ) where

import Control.Concurrent.MVar
import Control.Monad.Reader
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

import Ircd.Types

setNick :: PeerEnv -> PeerState -> String -> Env IO Bool
setNick penv pstate nick = asks envIrcdState >>= liftIO . flip modifyMVar setThatNick
  where
    -- TODO: we should only do that this way for clients.
    --       Servers require a different approach cause those are peers that hide multiple nicks!!!
    setThatNick :: IrcdState -> IO (IrcdState, Bool)
    setThatNick st = case M.lookup nick (ircdNicks st) of
        Just _ -> return (st, False)
        Nothing ->
            let pchans = peerChans pstate
                nicks = ircdNicks st
                nicksH = ircdNicksHistory st
                chans = ircdChans st
                st' = case peerNick pstate of
                    Just oldnick ->
                        st { ircdNicks = M.insert nick penv $ M.delete oldnick nicks
                           , ircdNicksHistory = M.insert nick penv nicksH
                           , ircdChans = foldl (\acc chan -> M.insert chan
                                                                      (nick : L.delete oldnick (fromMaybe [] $ M.lookup chan acc))
                                                                      acc)
                                               chans pchans }
                    Nothing -> st { ircdNicks = M.insert nick penv nicks
                                  , ircdNicksHistory = M.insert nick penv nicksH }
            in return (st', True)

