{-# LANGUAGE TemplateHaskell #-}

import Hedgehog
import Library
import Control.Monad (unless)
import System.Exit (exitFailure)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


tests :: IO Bool
tests =
  checkParallel $$discover

main :: IO ()
main = do
  result <- tests
  unless result exitFailure


tree :: Gen (Tree Int)
tree =
  Gen.recursive Gen.choice [
    pure Empty
  ] [
    Node <$> Gen.integral (Range.linear 0 1000) <*> tree <*> tree
  ]

prop_parses_any_tree :: Property
prop_parses_any_tree = property $ do
  t <- forAll tree
  snd (expand (collapse t)) === []

prop_isomorphic :: Property
prop_isomorphic = property $ do
  t <- forAll tree
  fst (expand (collapse t)) === t
