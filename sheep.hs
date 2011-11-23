module Main where

import Network.Multicast
import Network.Sendfile
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import qualified Network.Socket
import Network.Socket.ByteString
import Network hiding (accept)

import Data.ByteString(ByteString, empty)
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM

import Control.Concurrent
import System.Process -- hostname
import Control.Monad

import System.IO (hClose, openFile, IOMode(..))
import System.FilePath
import Control.Exception

-- 
import Data.Attoparsec
import Parser
import DataTypes

hostname :: IO String
hostname = readProcess "hostname" [] ""


getRandomName = do
  return "foobar"


clientDirect s who = do
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
                              case HM.lookup fid state of
                                Nothing -> return ()
                                Just h -> hClose h
                              go unc (HM.delete fid state)
      workWithMessage state unc (Chunk fid bs) = do
                              case HM.lookup fid state of
                                Nothing -> return ()
                                Just h -> BS.hPut h bs
                              go unc state
      workWithMessage state unc (Begin fid fname checksum) = do
                              case HM.lookup fid state of
                                Nothing -> do
                                  name <- getRandomName
                                  writeFile ("files" </> (name <.> "txt")) (show (who,fid,fname,checksum))
                                  h <- openFile ("files" </> (name <.> "dat")) WriteMode
                                  go unc (HM.insert fid h state)
                                Just _ -> go unc state -- TODO: report error

  go BS.empty HM.empty

main = withSocketsDo $ do
  h <- hostname

  let serverMulticast = do
        (sock, addr) <- multicastSender "224.0.0.99" 9999
        print "mcast sender hooked up"
        let loop = do
                 threadDelay (10^6)
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
        sock <- listenOn (PortNumber $ fromIntegral 9998)
        print "drct server hooked up"
        (forever $ loop sock) `finally` sClose sock
          where
            loop :: Socket -> IO ThreadId
            loop sock = do
                         (s,who) <- accept sock
                         forkIO (clientDirect s who)

  forkIO $ serverDirect
  forkIO $ serverMulticast
  forkIO $ clientMulticast

  forever $ (threadDelay (10^6))
