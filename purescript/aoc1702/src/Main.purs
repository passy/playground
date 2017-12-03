module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Data.Array (catMaybes, filter, index, zip)
import Data.Foldable (maximum, minimum, sum)
import Data.Int (fromString)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Monoid (mempty)
import Data.String (Pattern(..), split)
import Data.String.Utils (lines, words)
import Data.Tuple (uncurry)
import Node.Encoding (Encoding(UTF8))
import Node.FS (FS)
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)
import Node.Process (PROCESS)
import Node.Process as Process

main :: forall e. Eff (console :: CONSOLE, process :: PROCESS, exception :: EXCEPTION, fs :: FS | e) Unit
main = do
  (flip index 2) <$> Process.argv >>= case _ of
    Just arg -> run arg >>= show >>> log
    Nothing -> log $ "ERR: Provide a spreadsheet filename."

run :: FilePath -> forall e. Eff (fs :: FS, exception :: EXCEPTION | e) Int
run arg = do
  spreadsheet <- parseSpreadSheet <$> readTextFile UTF8 arg
  let lowest = fromMaybe 0 <<< minimum <$> spreadsheet
      highest = fromMaybe 0 <<< maximum <$> spreadsheet
  pure $ zip highest lowest <#> uncurry ((-)) # sum

parseSpreadSheet :: String -> Array (Array Int)
parseSpreadSheet input =
  filter ((/=) mempty) $ catMaybes <<< map fromString <<< words <$> lines input