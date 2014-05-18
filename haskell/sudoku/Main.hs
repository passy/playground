import Prelude
import Sudoku (solve)
import System.Environment (getArgs)
import Control.Parallel.Strategies (rpar, runEval, Eval)
import Data.Maybe (isJust)


main :: IO ()
main = do
  [f] <- getArgs
  file <- readFile f

  let puzzles = lines file
      solutions = runEval $ parMap solve puzzles
  print (length (filter isJust solutions))

-- A parallel version of map, dynamically creating sparks for each item
parMap :: (a -> b) -> [a] -> Eval [b]
parMap _ [] = return []
parMap f (a:as) = do
  b <- rpar (f a)
  bs <- parMap f as
  return (b:bs)
