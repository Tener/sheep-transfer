module Config where

fileReadChunkSize, sendHelloEvery :: (Num a) => a
sendHelloEvery = 10^7

fileReadChunkSize = 100 * 4096