import Control.Monad.IO.Class (MonadIO)
import Data.Conduit as C
import Data.Conduit.List as CL


multiplyC :: (MonadIO m) => C.Conduit Int m Int
multiplyC = CL.map (* 5)

main :: IO ()
main = do
    CL.sourceList [1..20] C.$$ multiplyC C.=$ CL.mapM_ (putStrLn . show)
