module Ircd.Command
    ( processPeerCommand
    ) where

import Control.Concurrent.MVar
import Control.Exception (IOException, catch)
import Control.Monad.Reader
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import qualified Network.IRC as IRC
import qualified Network.BSD ()
import Prelude hiding (catch)
import System.Log.Logger
import Text.Parsec

import Ircd.Server
import Ircd.Types
import Ircd.Utils

processPeerCommand :: IRC.Message -> PEnv (Env IO) Status
processPeerCommand msg = do
    penv <- ask
    pstateMV <- asks peerState
    pstate <- liftIO (readMVar pstateMV)
    let status = peerStatus pstate
    case IRC.msg_command msg of
        -- Connection and registration messages
        "PASS" -> do
            if status == UNREGISTERED
              then (case IRC.msg_params msg of
                pass:_ -> liftIO $ modifyMVar_ pstateMV (\st -> return st { peerPass = Just pass })
                [] -> replyStr "461" ["PASS", "Not enough parameters"])
              else replyStr "462" ["PASS", "You may not reregister"]
            return Continue
        "NICK" -> do
            when (status == UNREGISTERED) $ liftIO $ modifyMVar_ pstateMV (\st -> return st { peerStatus = REGISTERING })
            case IRC.msg_params msg of
                nick:stuff -> case parse nickName "" nick of
                    Right nick' -> do
                        -- If we got a valid nickname we try to set it in the server
                        success <- lift $ setNick penv pstate nick'
                        -- then we parse for the optional hopcount and build the msg we will relay to other peers
                        let hopcount = getHopcount pstate stuff
                            msg' = IRC.Message (IRC.msg_prefix msg) "NICK" [nick', show (hopcount + 1) ]
                        -- Finally we advertise to clients and servers
                        if success
                          then (do
                              -- We got the nick, so we update our peer state
                              liftIO $ modifyMVar_ pstateMV (\st -> return st { peerNick = Just nick' })
                              -- We get the list of peers on our chans so we can notify them of the nick change
                              -- TODO : check this, maybe it's not ok to send msg with hopcount to non server peers
                              getPeersOnMyChans pstate >>= liftIO . mapM_ (`sendTo` msg')
                              -- TODO : if we already have received a USER command from this directly connected client
                              -- we need to relay this USER the other servers now
                              when (status == REGISTERING && isJust (peerUser pstate)) $ liftIO $ modifyMVar_ pstateMV (\st -> return st { peerStatus = REGISTERED }))
                          else if peerIsServer pstate
                            then replyStr "436" ["NICK", nick' ++ " :Nickname collision KILL"] -- TODO send KILLs for both the old nickname and the new one
                            else replyStr "433" ["NICK", nick' ++ " :Nickname is already in use"]
                        return ()
                    Left _ -> replyStr "432" ["NICK", nick ++ " :Erroneus nickname"]
                [] -> replyStr "431" ["NICK", "No nickname given"]
            return Continue
        "USER" -> do
            when (status == UNREGISTERED) $ liftIO $ modifyMVar_ pstateMV (\st -> return st { peerStatus = REGISTERING })
            case IRC.msg_params msg of
                login:_:_:realname:_
                  | peerIsServer pstate -> notImplemented >>= \_ -> return () -- TODO check for nick in prefix too, as for hostname and servername
                  | status == REGISTERED -> replyStr "462" ["USER", "You may not reregister"]
                  | otherwise -> do
                      let hostname = "graou" --TODO liftIO . getNameInfo []
                      servername <- fmap configServerName (lift $ asks envConfig)
                      liftIO $ modifyMVar_ pstateMV (\st -> return st { peerUser = Just (login, hostname, servername, realname) })
                      -- TODO : If the client is fully registered we add a message prefix and we forward to all servers
                      when (status == REGISTERING && isJust (peerNick pstate)) $ liftIO $ modifyMVar_ pstateMV (\st -> return st { peerStatus = REGISTERED })
                _ -> replyStr "461" ["USER", "Not enough parameters"]
            return Continue
        "SERVER" -> do notImplemented; return Continue
        "OPER" -> do notImplemented; return Continue
        "QUIT" -> do
            let msg' = if peerIsServer pstate then msg
                         else IRC.Message (Just $ getPrefix pstate) (IRC.msg_command msg) $ if IRC.msg_params msg == [] then [ getNick pstate ]
                                                                                               else IRC.msg_params msg
            getPeersOnMyChans pstate >>= liftIO . mapM_ (`sendTo` msg')
            return . Exit . last $ IRC.msg_params msg'
        "SQUIT" -> do notImplemented; return Continue
        -- Messages available while registered
        _ | status /= REGISTERED -> do replyStr "451" ["You have not registered"]; return Continue
          | otherwise -> do
              case IRC.msg_command msg of
                  -- Channel operations
                  "JOIN" -> case IRC.msg_params msg of
                      [] -> replyStr "461" ["JOIN", "Not enough parameters"]
                      channelParams:lastMsgParams -> case parse joinChannelParams "" channelParams of
                          Right chanList -> do
                              let keyList = case lastMsgParams of
                                      [] -> []
                                      keyParams:_ -> case parse joinKeyParams "" keyParams of
                                          Right keyList' -> keyList'
                                          Left _ -> []
                              mapM_ (joinThatChan pstateMV pstate) . zip chanList $ keyList ++ repeat ""
                          Left _ -> replyStr "403" ["JOIN", "No such channel"]
                  "PART" -> notImplemented
                  "MODE" -> notImplemented
                  "TOPIC" -> notImplemented
                  "NAMES" -> notImplemented
                  "LIST" -> notImplemented
                  "INVITE" -> notImplemented
                  "KICK" -> notImplemented
                  -- Server queries and commands
                  "VERSION" -> notImplemented
                  "STATS" -> notImplemented
                  "LINKS" -> notImplemented
                  "TIME" -> notImplemented
                  "CONNECT" -> notImplemented
                  "TRACE" -> notImplemented
                  "ADMIN" -> notImplemented
                  "INFO" -> notImplemented
                  -- Sending messages
                  "PRIVMSG" -> notImplemented
                  "NOTICE" -> notImplemented
                  -- User based queries
                  "WHO" -> notImplemented
                  "WHOIS" -> notImplemented
                  "WHOWAS" -> notImplemented
                  -- Miscellaneous messages
                  "KILL" -> notImplemented
                  "PING" -> notImplemented
                  "PONG" -> notImplemented
                  "ERROR" -> notImplemented
                  -- Optional messages
                  "AWAY" -> notImplemented
                  "REHASH" -> notImplemented
                  "RESTART" -> notImplemented
                  "SUMMON" -> notImplemented
                  "USERS" -> notImplemented
                  "WALLOPS" -> notImplemented
                  "USERHOST" -> notImplemented
                  "ISON" -> notImplemented
                  _ -> liftIO $ errorM "Ircd.Command" $ "Invalid command in IRC message : " ++ IRC.msg_command msg
              return Continue
  where
    nickName = do
        n <- letter
        ick <- try (count 8 nickElt) <|> option [] (many1 nickElt)
        _ <- eof
        return $ n : ick
    joinChannelParams = do
        firstChan <- channelName
        chanList <- option [] $ many1 (char ',' >> channelName)
        return $ firstChan : chanList
    joinKeyParams = do
        firstKey <- channelKey
        keysList <- option [] $ many1 (char ',' >> channelKey)
        return $ firstKey : keysList
    channelName = do
        p <- oneOf "#&"
        t <- (try (count 200 chanElt) <|> option [] (many1 chanElt))
        return $ p : t
    channelKey = option [] (many1 nickElt)
    chanElt = noneOf " ,\a"
    nickElt = alphaNum <|> oneOf "-[]\\`^{}"
    getPeersOnMyChans :: PeerState -> PEnv (Env IO) [PeerEnv]
    getPeersOnMyChans pstate = do
        let chans = peerChans pstate
        stMVar <- lift (asks envIrcdState)
        penv <- ask
        peers <- liftIO $ withMVar stMVar
            (\st -> let nicks = L.nub . concat $ mapMaybe (flip M.lookup $ ircdChans st) chans
                    in return $ mapMaybe (flip M.lookup $ ircdNicks st) nicks)
        return $ if peerIsServer pstate
            then filter (/= penv) peers
            else peers
    joinThatChan :: MVar PeerState -> PeerState -> (String, String) -> PEnv (Env IO) ()
    joinThatChan pstateMV pstate (chan, _) = do   -- second param is the channel key provided
        stMVar <- lift (asks envIrcdState)
        members <- liftIO $ modifyMVar stMVar
            -- TODO : check channel mode invite-only, active bans on nick - username - hostname, is the key correct?
            (\st -> do
                let chans = ircdChans st
                    members = fromMaybe [] $ M.lookup chan chans
                    members' = getNick pstate : members
                -- TODO relay JOIN msg to all members
                return (st { ircdChans = M.insert chan members' chans }
                       , members'))
        liftIO $ modifyMVar_ pstateMV (\st -> return st { peerChans = chan : peerChans st })
        -- TODO if successfull : RPL_TOPIC and properly handle multilines RPL_NAMREPLY
        replyStr "351" [chan, "No topic is set"]
        replyStr "353" [chan, formatNickListRpl members]
        return ()
    getPrefix :: PeerState -> IRC.Prefix
    getPrefix pstate = let (login, hostname, _, _) = fromMaybe ("", "", "", "") $ peerUser pstate
        in IRC.NickName (getNick pstate) (Just login) (Just hostname)
    getNick :: PeerState -> String
    getNick = fromMaybe "" . peerNick
    getHopcount :: PeerState -> [IRC.Parameter] -> Int
    getHopcount pstate params
      | peerIsServer pstate = case params of
          hopC:_ -> case reads hopC :: [(Int, String)] of
              (hop,_):_ -> hop
              _ -> 0
          _ -> 0
      | otherwise = 0

    formatNickListRpl = unwords -- TODO : handle modes and multilines
    notImplemented = liftIO $ errorM "Ircd.Command" $ "Command not implemented : " ++ IRC.msg_command msg

replyStr :: IRC.Command -> [IRC.Parameter] -> PEnv (Env IO) ()
replyStr cmd params = do
    serverName <- fmap configServerName $ lift (asks envConfig)
    let msg = IRC.Message (Just $ IRC.Server serverName) cmd params
    ask >>= liftIO . flip sendTo msg

sendTo :: PeerEnv -> IRC.Message -> IO ()
sendTo penv msg = do
    let connhdl  = peerHandle penv
        tlsCtx   = peerTLSCtx penv
    liftIO $ (sendStr connhdl tlsCtx $ IRC.encode msg) `catch` \(_ :: IOException) -> return ()
    liftIO . debugM "Ircd.peer" $ "--> " ++ show msg

