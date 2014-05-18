import Prelude
import Sudoku (solve)
import System.Environment (getArgs)
import Control.Parallel.Strategies (rpar, rseq, runEval)
import Control.DeepSeq (force)
import Data.Maybe (isJust)


main :: IO ()
main = do
  [f] <- getArgs
  file <- readFile f

  let puzzles = lines file
      (as, bs) = splitAt (length puzzles `div` 2) puzzles
      solutions = runEval $ do
                    as' <- rpar (force (map solve as))
                    bs' <- rpar (force (map solve bs))
                    _ <- rseq as'
                    _ <- rseq bs'
                    return (as' ++ bs')

  print (length (filter isJust solutions))
