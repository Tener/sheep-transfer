module Config where

fileReadChunkSize, sendHelloEvery :: (Num a) => a
sendHelloEvery = 10^6

fileReadChunkSize = 100 * 4096