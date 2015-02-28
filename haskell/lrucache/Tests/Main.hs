module Main where

import Test.Hspec
import Test.QuickCheck
import Data.Cache.LRU (empty)
import qualified Data.Cache.LRU.Types as T

main :: IO ()
main = hspec $ do
    describe "LRU.empty" $ do
        it "always has a size of 0" $ property $
          \x -> x > 0 ==> (T.size $ empty x) == 0
        it "contains the right capacity" $ property $
          \x -> x > 0 ==> (T.capacity $ empty x) == x
