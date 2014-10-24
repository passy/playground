import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB


main :: IO ()
main = do
    runResourceT $ CB.sourceFile "shakespeare.txt" C.$$ CB.sinkFile "lulz"
