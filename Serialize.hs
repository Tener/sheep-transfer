{-# LANGUAGE TemplateHaskell #-}
module Serialize where

import Data.Serialize
import DataTypes

import Data.DeriveTH
import Data.Derive.Serialize

$( derive makeSerialize ''NetworkDirectMessage )