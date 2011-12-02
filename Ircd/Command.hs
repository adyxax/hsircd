module Ircd.Command
    ( processPeerCommand
    ) where

import Control.Concurrent.MVar
import Control.Monad.Reader
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import qualified Network.IRC as IRC
import qualified Network.BSD ()
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
                        -- TODO: we should only do that this way for clients.
                        --       Servers require a different approach cause those are peers that hide multiple nicks!!!
                        success <- lift (asks envIrcdState) >>= liftIO . flip modifyMVar (\st -> case M.lookup nick' (ircdNicks st) of
                            Just _ -> return (st, False)
                            Nothing ->
                                let pchans = peerChans pstate
                                    nicks = ircdNicks st
                                    chans = ircdChans st
                                    st' = case peerNick pstate of
                                        Just oldnick ->
                                            st { ircdNicks = M.insert nick' penv $ M.delete oldnick nicks
                                               , ircdChans = foldl (\acc chan -> M.insert chan
                                                                                          (nick' : L.delete oldnick (fromMaybe [] $ M.lookup chan acc))
                                                                                          acc)
                                                                   chans pchans }
                                        Nothing -> st { ircdNicks = M.insert nick' penv nicks }
                                in return (st', True))
                        -- Finally we advertise to clients and servers | WARNING : cannot test until JOIN is implemented
                        if success
                          then (do
                              liftIO $ modifyMVar_ pstateMV (\st -> return st { peerNick = Just nick' })
                              let chans = peerChans pstate
                              stMVar <- lift (asks envIrcdState)
                              peers <- liftIO $ withMVar stMVar
                                  (\st -> let nicks = L.nub . concat $ mapMaybe (flip M.lookup $ ircdChans st) chans
                                          in return $ mapMaybe (flip M.lookup $ ircdNicks st) nicks)
                              let peers' = if peerIsServer pstate
                                             then filter (/= penv) peers
                                             else peers
                              -- TODO : check this, maybe it's not ok to send msg with hopcount to non server peers
                              liftIO $ mapM_ (`sendTo` msg') peers'
                              -- TODO : if we already have received a USER command from this directly connected client
                              -- we need to relay this USER the other servers now
                              when (status == REGISTERING && peerUser pstate /= Nothing) $ liftIO $ modifyMVar_ pstateMV (\st -> return st { peerStatus = REGISTERED }))
                          else if peerIsServer pstate
                            then (replyStr "436" ["NICK", nick' ++ " :Nickname collision KILL"]
                                -- TODO send KILLs for both the old nickname and the new one
                                )
                            else replyStr "433" ["NICK", nick' ++ " :Nickname is already in use"]
                        return ()
                    Left _ -> replyStr "432" ["NICK", nick ++ " :Erroneus nickname"]
                [] -> replyStr "431" ["NICK", "No nickname given"]
        "USER" -> do
            when (status == UNREGISTERED) $ liftIO $ modifyMVar_ pstateMV (\st -> return st { peerStatus = REGISTERING })
            case IRC.msg_params msg of
                login:_:_:realname:_ -> if peerIsServer pstate
                  then return () -- TODO check for nick in prefix too, as for hostname and servername
                  else  if (status == REGISTERED)
                    then replyStr "462" ["USER", "You may not reregister"]
                    else (do
                      let hostname = "graou" --TODO liftIO . getNameInfo []
                      servername <- fmap configServerName (lift $ asks envConfig)
                      liftIO $ modifyMVar_ pstateMV (\st -> return st { peerUser = Just $ (login, hostname, servername, realname) })
                      -- TODO : If the client is fully registered we add a message prefix and we forward to all servers
                      when (status == REGISTERING && peerNick pstate /= Nothing) $ liftIO $ modifyMVar_ pstateMV (\st -> return st { peerStatus = REGISTERED }))
                _ -> replyStr "461" ["USER", "No enough parameters"]
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

