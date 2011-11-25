{-# LANGUAGE OverloadedStrings #-}

module ProtocolWriter where

import DataTypes
import Data.Serialize
import qualified Data.ByteString as BS
import Data.ByteString.Char8 () -- IsString intance

putByteStringLen bs = do
  putWord64le (fromIntegral (BS.length bs))
  putByteString bs
  
networkMessagePut :: Putter NetworkDirectMessage

networkMessagePut (Begin fid fname checksum) = do
  putByteString "begin"
  putWord64le $ fromIntegral fid
  putByteStringLen fname
  putByteStringLen checksum

networkMessagePut (Chunk fid chunk) = do
  putByteString "chunk"
  putWord64le $ fromIntegral fid
  putByteStringLen chunk

networkMessagePut (Finished fid) = do
  putByteString "finished"
  putWord64le $ fromIntegral fid

networkMessagePut Quit = do
  putByteString "quit"


