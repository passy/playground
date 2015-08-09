{-# LANGUAGE DeriveFunctor #-}

import Prelude
import Data.Sequence (ViewL(..))
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad.Loops

data Tree a = Node a (Tree a) (Tree a) | Empty
  deriving (Show, Functor)

traverseDF :: Tree a -> [a]
traverseDF Empty = []
traverseDF (Node v l r) =
  [v] ++ traverseDF l ++ traverseDF r

-- Imperative first.
traverseBF :: Tree a -> [a]
traverseBF Empty = []
traverseBF (Node v l r) = (flip execState) (Seq.singleton v) $ do
  -- TODO: Use this, needs to be part of the state, though.
  let visited = Set.empty :: Set.Set a

  -- While the queue is not empty
  whileM (not . Seq.null <$> get) $ do
    -- Get the queue
    q <- get
    -- Get its head and tail
    let (n :< ns) = Seq.viewl q
    -- XXX: Set the tail as new queue
    -- Should have the adjacent nodes.
    put ns
    return $ Seq.singleton v

main :: IO ()
main = do
  let t = Node "A" (Node "B" (Node "C" Empty Empty) Empty) (Node "D" Empty (Node "E" (Node "F" Empty Empty) Empty))
  -- print $ traverseDF t
  print $ traverseBF t
