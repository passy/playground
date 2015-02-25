module Data.Cache.LRU ( empty ) where

import           Control.Applicative ((<$>))
import           Data.Hashable       (Hashable, hash)
import qualified Data.HashPSQ        as HashPSQ
import           Data.IORef          (IORef, newIORef, atomicModifyIORef')
import           Data.Int            (Int64)
import           Data.Maybe          (isNothing)
import qualified Data.Vector         as V
import           Prelude             hiding (lookup)

import qualified Data.Cache.LRU.Types as T

empty :: Int -> T.Cache k v
empty capacity
    | capacity < 1 = error "LRU.empty: capacity < 1"
    | otherwise    = T.Cache
        { T.capacity = capacity
        , T.size     = 0
        , T.tick     = 0
        , T.queue    = HashPSQ.empty
        }
