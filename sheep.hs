module Main where

import Network.Multicast
import Network.Sendfile
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import qualified Network.Socket
import Network.Socket.ByteString
import Network hiding (accept)

import Data.ByteString(ByteString, empty)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS_UTF8
import qualified Data.HashMap.Strict as HM

import Control.Concurrent
import System.Process -- hostname
import Control.Monad

import System.IO (hClose, openFile, IOMode(..))
import System.FilePath
import Control.Exception
-- import Data.Attoparsec
import System.Random
import Data.Serialize --  (runPut, runGetPartial, get)

-- 
import ProtocolWriter
import Parser
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
        bs <- recv s 4096
        print bs
        return bs

  go (Just HM.empty) (Partial (runGetPartial get))
  
{-
clientDirect' s who = do
  print "a new direct client!"
  print who
  let go unconsumed state = do
       res <- parseWith (recv s 4096) networkMessageParser unconsumed
       case res of
         Fail a b c -> error (show ("clientDirect: Fail", a, b, c))
         Partial _p -> error (show ("clientDirect: Partial"))
         Done unconsumed'2 msg -> workWithMessage state unconsumed'2 msg 

      workWithMessage state _ Quit = do
                              print ("Quitting...",who)
                              mapM_ hClose (HM.elems state)
      workWithMessage state unc (Finished fid) = do
                              print ("Finished", fid)
                              case HM.lookup fid state of
                                Nothing -> return ()
                                Just h -> hClose h
                              go unc (HM.delete fid state)
      workWithMessage state unc (Chunk fid bs) = do
                              print ("Chunk", fid, BS.length bs)
                              case HM.lookup fid state of
                                Nothing -> return ()
                                Just h -> BS.hPut h bs
                              go unc state
      workWithMessage state unc (Begin fid fname checksum) = do
                              print ("Begin", fid, fname, checksum)
                              case HM.lookup fid state of
                                Nothing -> do
                                  name <- getRandomName
                                  writeFile ("files" </> (name <.> "txt")) (show (who,fid,fname,checksum))
                                  h <- openFile ("files" </> (name <.> "dat")) WriteMode
                                  go unc (HM.insert fid h state)
                                Just _ -> do
                                  print "ERROR: fid already allocated"
                                  go unc state -- TODO: report error

  go BS.empty HM.empty
-}


main = withSocketsDo $ do
  h <- hostname

  let serverMulticast = do
        (sock, addr) <- multicastSender "224.0.0.99" 9999
        print "mcast sender hooked up"
        let loop = do
                 threadDelay (10^8)
                 print "sending hello..."
                 Network.Socket.sendTo sock (show (Hello h)) addr
                 loop
        loop

      clientMulticast = do
        sock <- multicastReceiver "224.0.0.99" 9999
        print "mcast receiver hooked up"
        let loop = do
              (msg, _, addr) <- Network.Socket.recvFrom sock 1024
              print "got something..."
              print (msg, addr)
              loop
        loop
  
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
        putStrLn "Receiver: "
        receiverName <- getLine
                     
        handle <- openFile fileName ReadMode
        receiver <- connectTo receiverName (PortNumber 9998)
        fid <- randomRIO (0,(10^10))
        
        let send' msg = BS.hPut receiver (runPut (networkMessagePut msg))
            send msg = BS.hPut receiver (encode msg)
            sendTheFile = do
                           chunk <- BS.hGet handle 4096
                           when (not (BS.null chunk)) (send (Chunk fid chunk) >> sendTheFile)

        send (Begin fid (BS_UTF8.fromString fileName) BS.empty)
        sendTheFile
        send (Finished fid)
        send Quit

        hClose handle
        hClose receiver

  forkIO $ serverDirect
  forkIO $ serverMulticast
  forkIO $ clientMulticast

  forever $ senderDirect
