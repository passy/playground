{-# LANGUAGE NoImplicitPrelude #-}

-- Based on this chapter:
-- http://chimera.labs.oreilly.com/books/1230000000929/ch03.html#sec_using-parlist

module Main where

import Prelude

import System.IO
import Data.Array
import Data.Array.Unsafe as Unsafe
import Text.Printf
import Data.List
import Data.Function
import Data.Binary (decodeFile)
import Debug.Trace
import Control.Parallel.Strategies as Strategies
import Control.Monad.Par as Par
import Control.DeepSeq
import System.Environment
import Data.Time.Clock
import Control.Exception
import Control.Concurrent
import Control.Monad.ST
import Data.Array.ST
import System.Mem
import Data.Maybe

import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector
import Data.Vector (Vector)

data Point = Point !Double !Double
data PointSum = PointSum !Int !Double !Double

zeroPoint :: Point
zeroPoint = Point 0 0

sqDistance :: Point -> Point -> Double
sqDistance (Point x1 y1) (Point x2 y2) = ((x1 - x2)^2) + ((y1 - y2)^2)

addToPointSum :: PointSum -> Point -> PointSum
addToPointSum (PointSum count xs ys) (Point x y) =
    PointSum (count + 1) (xs + x) (ys + y)

pointSumToCluster :: Int -> PointSum -> Cluster
pointSumToCluster i (PointSum count xs ys) =
    Cluster {
        clId = i,
        clCent = Point (xs / fromIntegral count) (ys / fromIntegral count)
    }


data Cluster = Cluster {
    clId :: Int,
    clCent :: Point
}

assign :: Int -> [Cluster] -> [Point] -> Vector PointSum
assign nclusters clusters points = Vector.create $ do
        vec <- MVector.replicate nclusters (PointSum 0 0 0)
        let
            addpoint p = do
                let c = nearest p
                let cid = clId c
                ps <- MVector.read vec cid
                MVector.write vec cid $! addToPointSum ps p

        mapM_ addpoint points
        return vec
    where
        nearest p = fst $ minimumBy (compare `on` snd)
            [(c, sqDistance (clCent c) p) | c <- clusters]

makeNewCluster :: Vector PointSum -> [Cluster]
makeNewCluster vec = [ pointSumToCluster i ps
    | (i, ps@(PointSum count _ _)) <- zip [0..] (Vector.toList vec),
    count > 0 ]
