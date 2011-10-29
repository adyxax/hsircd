module Ircd.Utils
    ( addThreadIdToQuitMVar
    , delThreadIdFromQuitMVar
    , initSocket
    , initTLSEnv
    , readCertificate
    , readPrivateKey
    , sendStr
    , setGlobalQuitMVar
    ) where

import Control.Concurrent
import Control.Monad.Reader
import qualified Crypto.Cipher.RSA as RSA
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.UTF8 as L8
import qualified Data.Certificate.KeyRSA as KeyRSA
import Data.Certificate.PEM
import Data.Certificate.X509
import Data.List
import Network
import Network.Socket
import Network.TLS
import System.IO
import System.Log.Logger

import Ircd.Config
import Ircd.Types

-- utility functions
addThreadIdToQuitMVar :: ThreadId -> Env IO ()
addThreadIdToQuitMVar thrId = do
    threadIdsMv <- asks envThreadIdsMv
    liftIO $ modifyMVar_ threadIdsMv (\l -> return $ thrId:l)

delThreadIdFromQuitMVar :: ThreadId -> Env IO ()
delThreadIdFromQuitMVar thrId = do
    threadIdsMv <- asks envThreadIdsMv
    liftIO $ modifyMVar_ threadIdsMv (\l -> return $ delete thrId l)

setGlobalQuitMVar :: Status -> Env IO ()
setGlobalQuitMVar status = do
    quitMv <- asks envQuitMv
    liftIO $ putMVar quitMv status

-- Helpers
sendStr :: Handle -> Maybe (TLSCtx Handle) -> String -> IO ()
sendStr _ (Just ctx) msg = sendData ctx (L8.fromString $ msg ++ "\r\n")
sendStr handle Nothing msg = hPutStrLn handle (msg ++ "\r\n")

-- Socket utils
initSocket :: Listen -> IO (Socket)
initSocket (Listen hostname port) = do
    liftIO $ infoM "Ircd.Core" $ "Listening on " ++ hostname ++ ":" ++ port
    addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
    let serveraddr = head addrinfos
    mySocket <- socket (addrFamily serveraddr) Stream defaultProtocol
    setSocketOption mySocket ReuseAddr 1
    bindSocket mySocket (addrAddress serveraddr)
    listen mySocket 10
    return mySocket

-- TLS utils
initTLSEnv :: TLSConfig -> IO (TLSParams)
initTLSEnv tls = do
    infoM "Ircd.Core" "snif"
    let certFile = tlsCert tls
        keyFile  = tlsKey tls
        verify   = tlsVerify tls
    -- TODO : exception on loading keys
    infoM "Ircd.Core" "snif"
    cert <- readCertificate certFile
    infoM "Ircd.Core" "snif"
    pk   <- readPrivateKey keyFile
    infoM "Ircd.Core" "snif"
    return $ defaultParams { pConnectVersion = TLS12
                           , pWantClientCert = verify
                           , pCertificates = [(cert, Just pk)] }

readCertificate :: FilePath -> IO X509
readCertificate filepath = do
    content <- B.readFile filepath
    let certdata = case parsePEMCert content of
            Nothing -> error ("no valid certificate section")
            Just x  -> x
    let cert = case decodeCertificate $ L.fromChunks [certdata] of
            Left err -> error ("cannot decode certificate: " ++ err)
            Right x  -> x
    return cert

readPrivateKey :: FilePath -> IO PrivateKey
readPrivateKey filepath = do
    content <- B.readFile filepath
    let pkdata = case parsePEMKeyRSA content of
            Nothing -> error ("no valid RSA key section")
            Just x  -> L.fromChunks [x]
    let pk = case KeyRSA.decodePrivate pkdata of
            Left err -> error ("cannot decode key: " ++ err)
            Right x  -> PrivRSA $ RSA.PrivateKey
                { RSA.private_sz   = fromIntegral $ KeyRSA.lenmodulus x
                , RSA.private_n    = KeyRSA.modulus x
                , RSA.private_d    = KeyRSA.private_exponant x
                , RSA.private_p    = KeyRSA.p1 x
                , RSA.private_q    = KeyRSA.p2 x
                , RSA.private_dP   = KeyRSA.exp1 x
                , RSA.private_dQ   = KeyRSA.exp2 x
                , RSA.private_qinv = KeyRSA.coef x
                }
    return pk

