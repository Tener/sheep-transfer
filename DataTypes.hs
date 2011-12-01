{-# LANGUAGE FunctionalDependencies #-}

module DataTypes where

import Data.ByteString(ByteString)
import System.FilePath (FilePath)
import Network.Socket(SockAddr)

data NetworkMulticastMessage = Hello String | Goodbye String
                             deriving (Eq,Show,Read,Ord)

type FileId = Integer

data NetworkDirectMessage = Begin FileId ByteString ByteString -- file id, filename, checksum
                          | Chunk FileId Double ByteString -- file id, progress, file chunk (small, like 1024 * 8)
                          | Finished FileId
                          | Quit
                            deriving (Eq,Show,Read,Ord)


type ProgressCallback = Double -> IO ()
type FinishedCallback = IO ()
type StartCallback = FilePath -> FilePath -> HostName -> IO (ProgressCallback, FinishedCallback)

type NewConnectionCallback = Address -> IO NewFileCallback
type NewFileCallback = FileId -> ByteString -> IO (ProgressFileCallback, FinishedFileCallback)
type ProgressFileCallback = Double -> IO ()
type FinishedFileCallback = IO ()

type Peer = (Nick,Address)
type Nick = String
type Address = SockAddr
type HostName = String

class SheepTransferProtocol server server_config | server -> server_config where
    startServer :: server_config -> IO server
    stopServer :: server -> IO ()
    setNick :: server -> String -> IO ()
    getPeers :: server -> IO [Peer]
    sendFilePeer :: server -> FilePath -> HostName -> ProgressCallback -> FinishedCallback -> IO ()
    newConnectionCallback :: server -> NewConnectionCallback -> IO ()
