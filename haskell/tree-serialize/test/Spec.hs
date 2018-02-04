{-# LANGUAGE TemplateHaskell #-}

import           Hedgehog
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
