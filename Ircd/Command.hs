module Ircd.Command
    ( processPeerCommand
    ) where

import Control.Concurrent.MVar
import Control.Monad.Reader
import qualified Network.IRC as IRC
import System.Log.Logger

import Ircd.Types
import Ircd.Utils

processPeerCommand :: IRC.Message -> PEnv (Env IO) ()
processPeerCommand msg = do
    pstateMV <- asks peerState
    pstate <- liftIO (readMVar pstateMV)
    let status = peerStatus pstate
    case IRC.msg_command msg of
        "PASS" -> if status == UNREGISTERED
                    then (case IRC.msg_params msg of
                            pass:_ -> liftIO $ modifyMVar_ pstateMV (\st -> return st { peerPass = Just pass })
                            [] -> replyStr "461" ["PASS", "Not enough parameters"])
                    else replyStr "462" ["PASS", "You may not reregister"]
        "NICK" -> liftIO $ errorM "Ircd.Command" $ "Command not implemented : " ++ IRC.msg_command msg
        "USER" -> liftIO $ errorM "Ircd.Command" $ "Command not implemented : " ++ IRC.msg_command msg
        "SERVER" -> liftIO $ errorM "Ircd.Command" $ "Command not implemented : " ++ IRC.msg_command msg
        "OPER" -> liftIO $ errorM "Ircd.Command" $ "Command not implemented : " ++ IRC.msg_command msg
        "QUIT" -> liftIO $ errorM "Ircd.Command" $ "Command not implemented : " ++ IRC.msg_command msg
        "SQUIT" -> liftIO $ errorM "Ircd.Command" $ "Command not implemented : " ++ IRC.msg_command msg
        "JOIN" -> liftIO $ errorM "Ircd.Command" $ "Command not implemented : " ++ IRC.msg_command msg
        "PART" -> liftIO $ errorM "Ircd.Command" $ "Command not implemented : " ++ IRC.msg_command msg
        "MODE" -> liftIO $ errorM "Ircd.Command" $ "Command not implemented : " ++ IRC.msg_command msg
        "TOPIC" -> liftIO $ errorM "Ircd.Command" $ "Command not implemented : " ++ IRC.msg_command msg
        "NAMES" -> liftIO $ errorM "Ircd.Command" $ "Command not implemented : " ++ IRC.msg_command msg
        "LIST" -> liftIO $ errorM "Ircd.Command" $ "Command not implemented : " ++ IRC.msg_command msg
        "INVITE" -> liftIO $ errorM "Ircd.Command" $ "Command not implemented : " ++ IRC.msg_command msg
        "KICK" -> liftIO $ errorM "Ircd.Command" $ "Command not implemented : " ++ IRC.msg_command msg
        "VERSION" -> liftIO $ errorM "Ircd.Command" $ "Command not implemented : " ++ IRC.msg_command msg
        "STATS" -> liftIO $ errorM "Ircd.Command" $ "Command not implemented : " ++ IRC.msg_command msg
        "LINKS" -> liftIO $ errorM "Ircd.Command" $ "Command not implemented : " ++ IRC.msg_command msg
        "TIME" -> liftIO $ errorM "Ircd.Command" $ "Command not implemented : " ++ IRC.msg_command msg
        "CONNECT" -> liftIO $ errorM "Ircd.Command" $ "Command not implemented : " ++ IRC.msg_command msg
        "TRACE" -> liftIO $ errorM "Ircd.Command" $ "Command not implemented : " ++ IRC.msg_command msg
        "ADMIN" -> liftIO $ errorM "Ircd.Command" $ "Command not implemented : " ++ IRC.msg_command msg
        "INFO" -> liftIO $ errorM "Ircd.Command" $ "Command not implemented : " ++ IRC.msg_command msg
        "PRIVMSG" -> liftIO $ errorM "Ircd.Command" $ "Command not implemented : " ++ IRC.msg_command msg
        "NOTICE" -> liftIO $ errorM "Ircd.Command" $ "Command not implemented : " ++ IRC.msg_command msg
        "WHO" -> liftIO $ errorM "Ircd.Command" $ "Command not implemented : " ++ IRC.msg_command msg
        "WHOIS" -> liftIO $ errorM "Ircd.Command" $ "Command not implemented : " ++ IRC.msg_command msg
        "WHOWAS" -> liftIO $ errorM "Ircd.Command" $ "Command not implemented : " ++ IRC.msg_command msg
        "KILL" -> liftIO $ errorM "Ircd.Command" $ "Command not implemented : " ++ IRC.msg_command msg
        "PING" -> liftIO $ errorM "Ircd.Command" $ "Command not implemented : " ++ IRC.msg_command msg
        "PONG" -> liftIO $ errorM "Ircd.Command" $ "Command not implemented : " ++ IRC.msg_command msg
        "ERROR" -> liftIO $ errorM "Ircd.Command" $ "Command not implemented : " ++ IRC.msg_command msg
        "AWAY" -> liftIO $ errorM "Ircd.Command" $ "Command not implemented : " ++ IRC.msg_command msg
        "REHASH" -> liftIO $ errorM "Ircd.Command" $ "Command not implemented : " ++ IRC.msg_command msg
        "RESTART" -> liftIO $ errorM "Ircd.Command" $ "Command not implemented : " ++ IRC.msg_command msg
        "SUMMON" -> liftIO $ errorM "Ircd.Command" $ "Command not implemented : " ++ IRC.msg_command msg
        "USERS" -> liftIO $ errorM "Ircd.Command" $ "Command not implemented : " ++ IRC.msg_command msg
        "WALLOPS" -> liftIO $ errorM "Ircd.Command" $ "Command not implemented : " ++ IRC.msg_command msg
        "USERHOST" -> liftIO $ errorM "Ircd.Command" $ "Command not implemented : " ++ IRC.msg_command msg
        "ISON" -> liftIO $ errorM "Ircd.Command" $ "Command not implemented : " ++ IRC.msg_command msg
        _ -> liftIO $ errorM "Ircd.Command" $ "Invalid command in IRC message : " ++ IRC.msg_command msg

replyStr :: IRC.Command -> [IRC.Parameter] -> PEnv (Env IO) ()
replyStr cmd params = do
    penv <- ask
    serverName <- fmap configServerName $ lift (asks envConfig)
    let connhdl  = peerHandle penv
        tlsCtx   = peerTLSCtx penv
        msg = IRC.Message (Just $ IRC.Server serverName) cmd params
    liftIO . sendStr connhdl tlsCtx $ IRC.encode msg
    liftIO . debugM "Ircd.peer" $ "--> " ++ show msg


