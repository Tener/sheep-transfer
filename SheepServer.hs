{-# LANGUAGE ViewPatterns, MultiParamTypeClasses #-}

module SheepServer ( SheepServer, SheepConfig(..), debugSheepServer) where

import DataTypes
import Control.Concurrent.STM
import Control.Concurrent

import Data.Default


import Network.Multicast
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import qualified Network.Socket
import Network.Socket.ByteString
import Network hiding (accept)

import Data.ByteString(ByteString, empty)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS_UTF8
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict(HashMap)

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import System.Process
import System.IO (hClose, openFile, IOMode(..), hTell, hFileSize, Handle)
import Data.Ratio ((%))
import System.FilePath
import System.Random


import Data.Serialize

import Config

--
import DataTypes
import Serialize ()
import Data.Maybe(fromMaybe)

data SheepServer = SheepServer { mcastServerThread, mcastClientThread, directServerThread :: ThreadId
                               , nickV :: TVar String
                               , peersV :: TVar (HashMap String SockAddr)
                               , newConnCB :: TVar NewConnectionCallback
                               , peersChangedCB :: TVar (IO ())
                               }

type StateType = (HM.HashMap FileId (Handle, ProgressFileCallback, FinishedFileCallback))

data SheepConfig = SheepConfig {
    confMPort :: (Maybe Int)    -- multicast port, defaults to 9999
    , confDPort :: (Maybe Int)    -- tcp port, defaults to 9998
    , confMGroup :: (Maybe String) -- multicast group, defaults to 224.0.0.99
    , confNick :: (Maybe String) -- nick, defaults to hostname
    , confNewConnCb :: NewConnectionCallback -- what to do on new connection
    , peersChangedCb :: IO ()
    }

instance Default SheepConfig where
    def = let cb host = do
                let newFileCb fid fp = do
                        let progress i = print ("progress", i, fid, fp, host)
                            finished = print ("finished", fid, fp, host)
                        print ("new file", fid, fp, host)
                        return (progress,finished)
                print host
                return newFileCb in

          SheepConfig Nothing Nothing Nothing Nothing cb (print "Peers changed...")

instance SheepTransferProtocol SheepServer SheepConfig where
    startServer = start

    stopServer ss = do
      killThread (mcastClientThread ss)
      killThread (mcastServerThread ss)
      killThread (directServerThread ss)

    setNick (nickV -> nv) n = atomically $ writeTVar nv n

    getPeers (peersV -> pv) = fmap HM.toList (readTVarIO pv)

    sendFilePeer ss filepath hostname onProgress onFinished = do
        handle <- openFile filepath ReadMode
        receiver <- connectTo hostname (PortNumber 9998)
        fid <- randomRIO (0,(10^10))
        fsize <- hFileSize handle

        let send msg = BS.hPut receiver (encode msg)
            sendTheFile = do
                           chunk <- BS.hGet handle fileReadChunkSize
                           fpos <- hTell handle
                           let p = 100 * (fromRational $ fpos % fsize :: Double)
                           onProgress p
                           when (not (BS.null chunk)) (send (Chunk fid p chunk) >> sendTheFile)

        send (Begin fid (BS_UTF8.fromString filepath) BS.empty)
        sendTheFile
        send (Finished fid)
        send Quit

        onFinished

        hClose handle
        hClose receiver

    newConnectionCallback (newConnCB -> var) cb = atomically $ writeTVar var cb

hostname :: IO String
hostname = (takeWhile (/= '\n')) `fmap` (readProcess "hostname" [] "")


getRandomName = replicateM 10 (randomRIO ('a','z'))


clientDirect :: NewConnectionCallback -> Socket -> SockAddr -> IO ()
clientDirect cb s who = do
  newFileCallback <- cb who

  let go Nothing _ = return ()
      go (Just state) (Fail err) = error (show ("clientDirect: Fail",err))
      go (Just state) (Partial resume) = go (Just state) . resume =<< getMore
      go (Just state) (Done msg unconsumed) = do
                       state' <- consumeMessage state msg
                       go state' (runGetPartial (get :: Get NetworkDirectMessage) unconsumed)

      consumeMessage :: StateType -> NetworkDirectMessage -> IO (Maybe StateType)
      consumeMessage state Quit = do
                              print ("Quitting...",who)
                              mapM_ (\ (h, _, finCb) -> hClose h >> finCb) (HM.elems state)
                              return Nothing
      consumeMessage state (Finished fid) = do
                              print ("Finished", fid)
                              case HM.lookup fid state of
                                Nothing -> return ()
                                Just (h, _, finishedCb) -> do
                                        hClose h
                                        finishedCb
                                        return ()
                              return (Just $ HM.delete fid state)
      consumeMessage state (Chunk fid p bs) = do
                              let len = BS.length bs
                              print ("Chunk", fid, p, len)
                              case HM.lookup fid state of
                                Nothing -> return ()
                                Just (h, progCb, _) -> do
                                         BS.hPut h bs
                                         progCb p -- len
                              return (Just state)
      consumeMessage state (Begin fid fname checksum) = do
                              print ("Begin", fid, fname, checksum)
                              case HM.lookup fid state of
                                Nothing -> do
                                  name <- getRandomName
                                  let n1 = ("files" </> (name <.> "txt"))
                                      n2 = ("files" </> (name <.> "dat"))
                                  writeFile n1 (show (who,fid,fname,checksum))
                                  h <- openFile n2 WriteMode
                                  print ("File names", n1, n2)
                                  (progCb, finishedCb) <- newFileCallback fid fname
                                  let val = (h, progCb, finishedCb)
                                  return (Just $ HM.insert fid val state)
                                Just _ -> do
                                  print "ERROR: fid already allocated"
                                  return (Just state)

      getMore :: IO ByteString
      getMore = do
        bs <- recv s (10 * 1024)
        putStr "."
        -- print ("Received bytes",(BS.length bs))
        return bs

  go (Just HM.empty :: Maybe StateType) (Partial (runGetPartial get))

-- bcastAddr = "5.255.255.255"
mcastAddr = "224.0.0.99"
mcastBindAddr = "0.0.0.0"

fromMaybeM _ (Just x) = return x
fromMaybeM act Nothing = act

start (SheepConfig 
       (fromMaybe 9999 -> mport) 
       (fromMaybe 9998 -> dport)
       (fromMaybe mcastAddr -> maddr)
       mnick newConnCb peersChangedCb) = withSocketsDo $ do

  newConn <- newTVarIO newConnCb
  nick <- newTVarIO =<< fromMaybeM hostname mnick
  peers <- newTVarIO HM.empty
  peersChanged <- newTVarIO peersChangedCb

  let serverMulticast = do
        (sock, addr) <- multicastSender maddr (fromIntegral mport)
--        setInterface sock mcastBindAddr
--        setTimeToLive sock 150
        print "mcast sender hooked up"
        let loop = do
                 print "sending hello..."
                 h <- readTVarIO nick
                 Network.Socket.ByteString.sendTo sock (encode (Hello h)) addr
                 threadDelay sendHelloEvery
                 loop
        loop `finally` sClose sock

      clientMulticast = do
        sock <- multicastReceiver maddr (fromIntegral mport)
--        setInterface sock mcastBindAddr
--        setTimeToLive sock 150
        print "mcast receiver hooked up"
        let loop = do
              (msg, addr) <- Network.Socket.ByteString.recvFrom sock 4096
              print "got something..."
              print (msg, addr)
              case decode msg of
                Left err -> print ("Error",err)
                Right msg -> do
                     b <- consumeMulticastMsg addr msg
                     cb <- readTVarIO peersChanged
                     when b cb
              print =<< readTVarIO peers
              loop
        loop `finally` sClose sock

      consumeMulticastMsg addr (Hello nick) = atomically $
       do
         m <- readTVar peers
         let m' = (HM.insert nick addr m)
         writeTVar peers m'
         return (m' /= m)

      consumeMulticastMsg addr (Goodbye nick) = atomically $
       do
         m <- readTVar peers
         let m' = (HM.delete nick m)
         writeTVar peers m'
         return (m' /= m)

      serverDirect = do
        sock <- listenOn (PortNumber (fromIntegral dport))
        print "drct server hooked up"
        (forever $ loop sock) `finally` sClose sock
          where
            loop :: Socket -> IO ThreadId
            loop sock = do
                         (s,who) <- accept sock
                         cb <- readTVarIO newConn
                         forkIO (clientDirect cb s who)
   
  tid'1 <- forkIO $ serverDirect
  tid'2 <- forkIO $ clientMulticast
  tid'3 <- forkIO $ serverMulticast

  -- forever $ senderDirect

  return $ SheepServer tid'1 tid'2 tid'3 nick peers newConn peersChanged

debugSheepServer (SheepServer t1 t2 t3 nv pv _ _)  = do
  print(t1,t2,t3)
  n <- readTVarIO nv
  p <- readTVarIO pv
  print n
  print p
  
