module Main where

import Network.Multicast
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import qualified Network.Socket
import Network.Socket.ByteString
import Network hiding (accept)

import Data.ByteString(ByteString, empty)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS_UTF8
import qualified Data.HashMap.Strict as HM

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import System.Process
import System.IO (hClose, openFile, IOMode(..), hTell, hFileSize)
import Data.Ratio ((%))
import System.FilePath
import System.Random

import Data.Serialize

--
import DataTypes
import Serialize ()

hostname :: IO String
hostname = readProcess "hostname" [] ""


getRandomName = replicateM 10 (randomRIO ('a','z'))

clientDirect s who = do
  print "a new direct client!"
  print who

  let go Nothing _ = return ()
      go (Just state) (Fail err) = error (show ("clientDirect: Fail",err))
      go (Just state) (Partial resume) = go (Just state) . resume =<< getMore
      go (Just state) (Done msg unconsumed) = do
                       state' <- consumeMessage state msg
                       go state' (runGetPartial (get :: Get NetworkDirectMessage) unconsumed)

      consumeMessage state Quit = do
                              print ("Quitting...",who)
                              mapM_ hClose (HM.elems state)
                              return Nothing
      consumeMessage state (Finished fid) = do
                              print ("Finished", fid)
                              case HM.lookup fid state of
                                Nothing -> return ()
                                Just h -> hClose h
                              return (Just $ HM.delete fid state)
      consumeMessage state (Chunk fid bs) = do
                              print ("Chunk", fid, BS.length bs)
                              case HM.lookup fid state of
                                Nothing -> return ()
                                Just h -> BS.hPut h bs
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
                                  return (Just $ HM.insert fid h state)
                                Just _ -> do
                                  print "ERROR: fid already allocated"
                                  return (Just state)

      getMore :: IO ByteString
      getMore = do
        bs <- recv s (10 * 1024)
        print ("Received bytes",(BS.length bs))
        return bs

  go (Just HM.empty) (Partial (runGetPartial get))

-- bcastAddr = "5.255.255.255"
mcastAddr = "224.0.0.99"
mcastBindAddr = "0.0.0.0"

main = withSocketsDo $ do
  h <- (takeWhile (/= '\n')) `fmap` hostname

  peers <- newTVarIO HM.empty

  let serverMulticast = do
        (sock, addr) <- multicastSender mcastAddr 9999
        setInterface sock mcastBindAddr
        setTimeToLive sock 150
        print "mcast sender hooked up"
        let loop = do
                 print "sending hello..."
                 Network.Socket.ByteString.sendTo sock (encode (Hello h)) addr
                 threadDelay (10^8)
                 loop
        loop

      clientMulticast = do
        sock <- multicastReceiver mcastAddr 9999
        setInterface sock mcastBindAddr
        setTimeToLive sock 150
        print "mcast receiver hooked up"
        let loop = do
              (msg, addr) <- Network.Socket.ByteString.recvFrom sock 4096
              print "got something..."
              print (msg, addr)
              case decode msg of
                Left err -> print ("Error",err)
                Right msg -> consumeMulticastMsg addr msg
              print =<< readTVarIO peers
              loop
        loop

      consumeMulticastMsg addr (Hello nick) = atomically $
       do
         m <- readTVar peers
         writeTVar peers (HM.insert nick addr m)

      consumeMulticastMsg addr (Goodbye nick) = atomically $
       do
         m <- readTVar peers
         writeTVar peers (HM.delete nick m)

      serverDirect = do
        sock <- listenOn (PortNumber 9998)
        print "drct server hooked up"
        (forever $ loop sock) `finally` sClose sock
          where
            loop :: Socket -> IO ThreadId
            loop sock = do
                         (s,who) <- accept sock
                         forkIO (clientDirect s who)

      senderDirect = do
        putStrLn "Filename: "
        fileName <- getLine
        putStrLn "Known peers:"
        ps <- readTVarIO peers
        mapM_ (\ (nick,addr) -> do
                 putStr nick
                 putStr " -- "
                 putStrLn (show addr)) (HM.toList ps)
        putStrLn "Receiver: "
        receiverName <- getLine

        handle <- openFile fileName ReadMode
        receiver <- connectTo receiverName (PortNumber 9998)
        fid <- randomRIO (0,(10^10))

        fsize <- hFileSize handle

        let send msg = BS.hPut receiver (encode msg)
            sendTheFile = do
                           chunk <- BS.hGet handle 4096
                           fpos <- hTell handle
                           print ("File sending progress", fpos, fsize, 100 * (fromRational $ fpos % fsize :: Double))
                           when (not (BS.null chunk)) (send (Chunk fid chunk) >> sendTheFile)

        send (Begin fid (BS_UTF8.fromString fileName) BS.empty)
        sendTheFile
        send (Finished fid)
        send Quit

        hClose handle
        hClose receiver

  forkIO $ serverDirect
  forkIO $ clientMulticast
  forkIO $ serverMulticast

  forever $ senderDirect
