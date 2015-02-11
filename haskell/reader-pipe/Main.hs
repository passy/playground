{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, RankNTypes #-}

import Prelude hiding (mapM_)

import Data.Foldable (forM_)
import Data.Maybe (fromMaybe)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Resource (runResourceT)

import qualified Data.Text as T
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Text as CT

type ScoreMap = [(T.Text, Int)]


-- TODO: Let this take some scoring read from a Reader into account
mapWordsScore :: MonadIO m => ScoreMap -> C.Conduit T.Text m (T.Text, Int)
mapWordsScore scores = do
    res <- C.await
    forM_ res $ \word -> do
        let s = lookup word scores
        C.yield (word, fromMaybe 0 s)
        mapWordsScore scores

reduceScore :: MonadIO m => C.Sink (T.Text, Int) m Int
reduceScore = CL.fold (\a b -> a + snd b) 0

(-:) :: forall a b. a -> b -> (a, b)
(-:) = (,)

scoring :: ScoreMap
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
        -- Useful for debugging, limit to n lines.
        -- CL.isolate 16 C.$=
        CL.mapFoldable T.words C.$=
        mapWordsScore scoring C.$=
        reduceScore

    print res
