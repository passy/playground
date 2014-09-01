import GHC.IO.Exception (AssertionFailed(..))
import Control.Monad (unless)
import Control.Exception (throw)

main :: IO ()
main = do
    a <- getLine
    unless (take 1 a /= "a") (throw $ AssertionFailed "Never start with an a!")
