import Pipes
import qualified Pipes.Prelude as P


main :: IO ()
main = do
    runEffect $ each ["hello", "world"] >-> P.take 1 >-> P.stdoutLn
