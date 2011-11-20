module Ircd.Command
    ( processPeerCommand
    ) where

import Control.Concurrent.MVar
import Control.Monad.Reader
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import qualified Network.IRC as IRC
import System.Log.Logger
import Text.Parsec

import Ircd.Types
import Ircd.Utils

processPeerCommand :: IRC.Message -> PEnv (Env IO) ()
processPeerCommand msg = do
    penv <- ask
    pstateMV <- asks peerState
    pstate <- liftIO (readMVar pstateMV)
    let status = peerStatus pstate
    case IRC.msg_command msg of
        "PASS" -> if status == UNREGISTERED
                    then (case IRC.msg_params msg of
                            pass:_ -> liftIO $ modifyMVar_ pstateMV (\st -> return st { peerPass = Just pass })
                            [] -> replyStr "461" ["PASS", "Not enough parameters"])
                    else replyStr "462" ["PASS", "You may not reregister"]
        "NICK" -> do
            when (status == UNREGISTERED) $ liftIO $ modifyMVar_ pstateMV (\st -> return st { peerStatus = REGISTERING })
            case IRC.msg_params msg of
                nick:stuff -> case parse nickName "" nick of
                    Right nick' -> do
                        -- If we got a valid nickname, we parse for the optional hopcount
                        hopcount <- if peerIsServer pstate
                          then case stuff of
                              hopC:_ -> case reads hopC :: [(Int, String)] of
                                  (hop,_):_ -> return hop
                                  _ -> do liftIO . errorM "Ircd.Peer" $ "Bogus server peer " ++ show (peerClientAddr penv) ++ " :couldn't parse hopcount from server nick message : " ++ show stuff ; return 0
                              _ -> do liftIO . errorM "Ircd.Peer" $ "Bogus server peer " ++ show (peerClientAddr penv) ++ " :no hopcount provided" ; return 0
                          else return 0
                        let msg' = IRC.Message (IRC.msg_prefix msg) "NICK" [nick', show (hopcount + 1) ]
                        -- Then we try to set the nickname in the server state
                        success <- lift (asks envIrcdState) >>= liftIO . flip modifyMVar (\st -> case M.lookup nick' (ircdNicks st) of
                            Just _ -> return (st, False)
                            Nothing -> return (st { ircdNicks = M.insert nick' penv $ ircdNicks st }, True))
                        -- Finally we advertise to clients and servers | WARNING : cannot test until JOIN is implemented
                        if success
                          then (do
                              let chans = peerChans pstate
                              stMVar <- lift (asks envIrcdState)
                              peers <- liftIO $ withMVar stMVar
                                  (\st -> let nicks = L.nub . concat $ mapMaybe (flip M.lookup $ ircdChans st) chans
                                          in return $ mapMaybe (flip M.lookup $ ircdNicks st) nicks)
                              let peers' = if peerIsServer pstate
                                             then filter (/= penv) peers
                                             else peers
                              -- TODO : check this, maybe it's not ok to send msg with hopcount to non server peers
                              liftIO $ mapM_ (`sendTo` msg') peers')
                          else if peerIsServer pstate
                            then (replyStr "436" ["NICK", nick' ++ " :Nickname collision KILL"]
                                -- TODO send KILLs for both the old nickname and the new one
                                )
                            else replyStr "433" ["NICK", nick' ++ " :Nickname is already in use"]
                        return ()
                    Left _ -> replyStr "432" ["NICK", nick ++ " :Erroneus nickname"]
                [] -> replyStr "431" ["NICK", "No nickname given"]
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
  where
    nickName = do
        n <- letter
        ick <- try (count 8 nickElt) <|> option [] (many1 nickElt)
        _ <- eof
        return $ n : ick
    nickElt = alphaNum <|> oneOf "-[]\\`^{}"

replyStr :: IRC.Command -> [IRC.Parameter] -> PEnv (Env IO) ()
replyStr cmd params = do
    serverName <- fmap configServerName $ lift (asks envConfig)
    let msg = IRC.Message (Just $ IRC.Server serverName) cmd params
    ask >>= liftIO . flip sendTo msg

sendTo :: PeerEnv -> IRC.Message -> IO ()
sendTo penv msg = do
    let connhdl  = peerHandle penv
        tlsCtx   = peerTLSCtx penv
    liftIO . sendStr connhdl tlsCtx $ IRC.encode msg
    liftIO . debugM "Ircd.peer" $ "--> " ++ show msg

