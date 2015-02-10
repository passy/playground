{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, RankNTypes #-}

import Prelude hiding (mapM_)

import Data.Foldable (mapM_)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Resource (runResourceT)

import qualified Data.Text as T
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Text as CT


-- TODO: Let this take some scoring read from a Reader into account
mapWords :: MonadIO m => C.Conduit T.Text m (T.Text, Int)
mapWords = C.await >>= mapM_ (\w -> C.yield (w, 1)) >> mapWords

reduceScore :: MonadIO m => C.Sink (T.Text, Int) m Int
reduceScore = CL.fold (\a b -> a + snd b) 0

(-:) :: forall a b. a -> b -> (a, b)
(-:) = (,)

scoring :: [(T.Text, Int)]
scoring = [ "winters" -: 10
          , "pilgrimage" -: 8
          , "rug-headed" -: 5
          ]

main :: IO ()
main = do
    res <- runResourceT $
        CB.sourceFile "shakespeare.txt" C.$$
        CB.lines C.$=
        CT.decode CT.utf8 C.$=
        CL.filter (not . T.null) C.$=
        CL.mapFoldable T.words C.$=
        mapWords C.$=
        reduceScore

    print res
