import Prelude
import Sudoku (solve)
import System.Environment (getArgs)
import Control.Parallel.Strategies (using, parList, rseq)
import Data.Maybe (isJust)


main :: IO ()
main = do
  [f] <- getArgs
  file <- readFile f

  let puzzles = lines file
      solutions = map solve puzzles `using` parList rseq
  print (length (filter isJust solutions))
