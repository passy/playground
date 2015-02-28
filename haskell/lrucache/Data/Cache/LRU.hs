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

-- TODO: Should that constraint be part of Cache itself somehow?
trim :: (Hashable k, Ord k) => T.Cache k v -> T.Cache k v
trim c
    | T.tick c == maxBound =
        -- Taking the easy way out, just reset everything
        empty (T.capacity c)
        -- TODO: Use lenses here
    | T.size c > T.capacity c = c
        { T.size = T.size c - 1
        , T.queue = HashPSQ.deleteMin (T.queue c)
        }
    | otherwise = c
