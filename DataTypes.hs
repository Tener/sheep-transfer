module DataTypes where

import Data.ByteString(ByteString)

data NetworkMulticastMessage = Hello String | Goodbye String
                             deriving (Eq,Show,Read,Ord)

type FileId = Integer

data NetworkDirectMessage = Begin FileId ByteString ByteString -- filename, file id, checksum
                          | Chunk FileId ByteString -- file id, file chunk (small, like 1024 * 8)
                          | Finished FileId
                          | Quit
                            deriving (Eq,Show,Read,Ord)

