{-# LANGUAGE ScopedTypeVariables #-}
module Ircd.Peer
    ( defaultPeerState
    , handlePeerRequests
    ) where

import Control.Concurrent.MVar
import Control.Exception (IOException, catch)
import Control.Monad.Reader
import qualified Data.ByteString.Lazy.UTF8 as L
import Data.List (delete)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Network.IRC as IRC
import Network.TLS
import Prelude hiding (catch)
import System.IO
import System.Log.Logger
import Text.Parsec

import Ircd.Command
import Ircd.Types

defaultPeerState :: PeerState
defaultPeerState = PeerState
    { peerStatus = UNREGISTERED
    , peerPass   = Nothing
    , peerNick   = Nothing
    , peerUser   = Nothing
    , peerServer = False }

handlePeerRequests :: PeerEnv -> Env IO ()
handlePeerRequests peerEnv = do
    addIrcdPeer peerEnv
    env <- ask
    _ <- liftIO $ runReaderT (runReaderT (peerCore "") peerEnv) env `catch` return
    delIrcdPeer peerEnv
    liftIO $ infoM "Ircd.Peer" "Closing connection"
    liftIO . hClose $ peerHandle peerEnv

peerCore :: String -> PEnv (Env IO) IOException
peerCore buff = do
    peerEnv <- ask
    (msgs, tcpbuff) <- lift . liftIO $ botReader peerEnv buff
    mapM_ handleMessage msgs
    peerCore tcpbuff
  where
    botReader :: PeerEnv -> String -> IO ([String], String)
    botReader peerEnv tcpbuff = do
        let connhdl = peerHandle peerEnv
            ctx     = peerTLSCtx peerEnv
        str <- readThis connhdl ctx
        case parse messages [] (tcpbuff ++ str) of
            Right (msgs, trash) -> return (msgs, trash)
            Left err -> do
                errorM "Hsbot.Reader" $ "Reader decode error (" ++ show err ++ ") on " ++ str
                return ([], "")
    messages = do
        msgs <- option [] $ many1 message
        trash <- option "" $ many1 anyChar
        return (msgs, trash)
    message = do
        mess <- many1 $ noneOf "\r\n"
        end <- string "\r\n" <|> string "\r" <|> string "\n"
        return $ mess ++ end
    handleMessage :: String -> PEnv (Env IO) ()
    handleMessage str =
        case IRC.decode str of
            Just msg -> do
                liftIO . debugM "Hsbot.Reader" $ "<-- " ++ show msg
                processPeerCommand msg
            Nothing -> return ()
    readThis :: Handle -> Maybe (TLSCtx Handle) -> IO String
    readThis _ (Just ctx) = fmap L.toString (recvData ctx)
    readThis h Nothing = hGetLine h >>= \s -> return $ s ++ "\n"

addIrcdPeer :: PeerEnv -> Env IO ()
addIrcdPeer penv = asks envIrcdState >>= liftIO . flip modifyMVar_ (\state -> return state { ircdPeers = penv : ircdPeers state })

delIrcdPeer :: PeerEnv -> Env IO ()
delIrcdPeer penv = do
    pstate <- liftIO . takeMVar $ peerState penv
    asks envIrcdState >>= liftIO . readMVar >>= liftIO . print
    -- TODO : tell everyone about this peer quiting
    asks envIrcdState >>= liftIO . flip modifyMVar_ (\state -> return state { ircdPeers = delete penv $ ircdPeers state
                                                                            , ircdNicks = M.delete (fromMaybe "" $ peerNick pstate) $ ircdNicks state })

