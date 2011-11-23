{-# LANGUAGE OverloadedStrings #-}

module Parser where

import DataTypes
import Data.Attoparsec
import Data.Attoparsec.Binary
import Prelude hiding (take)
import qualified Data.ByteString as BS
import Control.Applicative

-- first number of bytes, then the number in 256-base
parseNumberVariable :: Parser Integer
parseNumberVariable = do
  n <- anyWord8 -- number of bytes following
  bs <- take (fromIntegral n)
  return (BS.foldl' (\ acc w8 -> (fromIntegral w8) + (acc * 256)) 0 bs)

beginParser = do
  string "begin"
  fieldLen'1 <- fromIntegral <$> parseNumberVariable
  fileName <- take fieldLen'1
  fileId <- parseNumberVariable
  fieldLen'2 <- fromIntegral <$> parseNumberVariable
  checksum <- take fieldLen'2
  
  return (Begin fileId fileName checksum)

chunkParser = do
  string "chunk"
  fileId <- parseNumberVariable
  len <- fromIntegral <$> parseNumberVariable
  bs <- take len
  return (Chunk fileId bs)

finishedParser = do
  string "finished"
  fileId <- parseNumberVariable
  return (Finished fileId)

quitParser = do
  string "quit"
  return Quit

networkMessageParser :: Parser NetworkDirectMessage
networkMessageParser = beginParser <|> chunkParser <|> finishedParser <|> quitParser
