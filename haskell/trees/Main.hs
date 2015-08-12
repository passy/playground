{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

import GHC.Generics (Generic)

import Prelude
import Data.Sequence (ViewL(..))
import Data.Monoid
import qualified Data.Sequence as Seq
import qualified Data.HashSet as Set
import Control.Monad.State
import Control.Monad.Loops
import Data.Hashable

data Tree a = Node a (Tree a) (Tree a) | Empty
  deriving (Show, Functor, Eq, Generic, Hashable)

adjacentNodes :: Tree a -> [Tree a]
adjacentNodes Empty = []
adjacentNodes (Node _ Empty Empty) = []
adjacentNodes (Node _ left Empty) = [left]
adjacentNodes (Node _ Empty right) = [right]
adjacentNodes (Node _ left right) = [left, right]

traverseDF :: Tree a -> [a]
traverseDF Empty = []
traverseDF (Node v l r) = [v] ++ traverseDF l ++ traverseDF r

-- Imperative first. (And I know that I'm vastly overcomplicating this, but
-- this has been an incredible learning experience.)
traverseBF :: (Hashable a, Eq a) => Tree a -> [a]
traverseBF Empty = []
traverseBF node = flip evalState (Seq.singleton node, Set.empty) $
  -- While the queue is not empty
  whileM (not . Seq.null . fst <$> get) $ do
    -- Get the state
    (queue, visited) <- get
    -- Pop the first item off the queue
    let (n@(Node v _ _) :< ns) = Seq.viewl queue
    -- Get all adjacent nodes that haven't been marked before
    let adj = Seq.fromList [ n' | n' <- adjacentNodes n, not $ Set.member n' visited ]
    -- Update the state
    put (ns <> adj, n `Set.insert` visited)
    -- Push this into the result
    return v

createTree :: Tree Char
createTree = Node 'A'
                (Node 'B'
                    (Node 'C' Empty Empty)
                    (Node 'D' Empty Empty)
                )
                (Node 'E'
                    (Node 'F' Empty Empty)
                    (Node 'G' Empty (Node 'H'
                        (Node 'I' Empty Empty)
                        Empty
                    ))
                )

main :: IO ()
main = do
  putStrLn "DFS:"
  print $ traverseDF createTree
  putStrLn "BFS:"
  print $ traverseBF createTree
