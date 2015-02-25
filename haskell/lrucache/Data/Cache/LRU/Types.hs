module Data.Cache.LRU.Types where

import Data.Int (Int64)
import qualified Data.HashPSQ as HashPSQ


type Priority = Int64

data Cache k v =
    Cache { capacity :: !Int
          , size     :: !Int
          , tick     :: !Priority
          , queue    :: !(HashPSQ.HashPSQ k Priority v)
          }
