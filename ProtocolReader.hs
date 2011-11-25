{-# LANGUAGE OverloadedStrings #-}

module ProtocolReader where

import DataTypes
import Data.Serialize
import qualified Data.ByteString as BS
import Data.ByteString.Char8 () -- IsString intance

networkMessageGet :: Get NetworkDirectMessage
networkMessageGet 
