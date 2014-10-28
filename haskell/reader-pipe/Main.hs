{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}


import Prelude hiding (mapM_)

import Data.Foldable (mapM_)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Resource (runResourceT)

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB


mapWords :: MonadIO m => C.Conduit T.Text m (T.Text, Int)
mapWords = C.await >>= mapM_ (\w -> C.yield (w, 1)) >> mapWords


reduceWords :: MonadIO m => C.Conduit (T.Text, Int) m [(T.Text, Int)]
reduceWords = do
    yo <- CL.fold (\a b -> a) []
    C.yield yo >> reduceWords

main :: IO ()
main = do
    result <- runResourceT $ CB.sourceFile "shakespeare.txt" C.$$
        CB.lines C.$=
        CL.map (T.strip . TE.decodeUtf8) C.$=
        CL.filter (not . T.null) C.$=
        CL.mapFoldable T.words C.$=
        mapWords C.$=
        reduceWords C.$=
        CL.take 5

    print result
