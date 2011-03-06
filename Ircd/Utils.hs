module Ircd.Utils
    ( addThreadIdToQuitMVar
    , delThreadIdFromQuitMVar
    , readCertificate
    , readPrivateKey
    , setGlobalQuitMVar
    ) where

import Control.Concurrent
import Control.Monad.Reader
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Certificate.KeyRSA as KeyRSA
import Data.Certificate.PEM
import Data.Certificate.X509
import Data.List

import Ircd.Types

-- | utility functions
addThreadIdToQuitMVar :: ThreadId -> Ircd IO ()
addThreadIdToQuitMVar thrId = do
    threadIdsMv <- asks envThreadIdsMv
    liftIO $ modifyMVar_ threadIdsMv (\l -> return $ thrId:l)

delThreadIdFromQuitMVar :: ThreadId -> Ircd IO ()
delThreadIdFromQuitMVar thrId = do
    threadIdsMv <- asks envThreadIdsMv
    liftIO $ modifyMVar_ threadIdsMv (\l -> return $ delete thrId l)

setGlobalQuitMVar :: IrcdStatus -> Ircd IO ()
setGlobalQuitMVar status = do
    quitMv <- asks envQuitMv
    liftIO $ putMVar quitMv status

-- | TLS utils
readCertificate :: FilePath -> IO (B.ByteString, X509)
readCertificate filepath = do
    content <- B.readFile filepath
    let certdata = case parsePEMCert content of
                        Nothing -> error ("no valid certificate section")
                        Just x  -> x
    let cert = case decodeCertificate $ L.fromChunks [certdata] of
                        Left err -> error ("cannot decode certificate: " ++ err)
                        Right x  -> x
    return (certdata, cert)

readPrivateKey :: FilePath -> IO (L.ByteString, KeyRSA.Private)
readPrivateKey filepath = do
    content <- B.readFile filepath
    let pkdata = case parsePEMKeyRSA content of
                        Nothing -> error ("no valid RSA key section")
                        Just x  -> L.fromChunks [x]
    let pk = case KeyRSA.decodePrivate pkdata of
                        Left err -> error ("cannot decode key: " ++ err)
                        Right x  -> x
    return (pkdata, pk)

