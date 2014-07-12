{-# LANGUAGE TypeFamilies, FlexibleContexts, ConstraintKinds, NoImplicitPrelude #-}
-- See
-- https://tel.github.io/2014/07/12/mutable_algorithms_in_immutable_languges_part_1/
module Main
where

import Prelude
import Control.Monad (unless)

-- | A monad providing an abstract interface over mutable memory,
--   think of Ref(erences) in terms of pointers. Vals are the values
--   referenced by a pointer.
class (Monad r, Eq (Ref r)) => Mem r where
    data family Ref r :: *
    type family Val r :: *

    -- | Reference a value by lifting it into the monad
    ref :: Val r -> r (Ref r)

    -- | Deference a reference
    deref :: Ref r -> r (Val r)

    -- | Set the value of a reference to a new value (side-effecting)
    set :: Ref r -> Val r -> r ()


-- | Alter the value of a `Mem` reference. Notice that this works for *any*
--   `Mem` monad.
alter :: Mem r => (Val r -> Val r) -> Ref r -> r ()
alter f r = do
    v <- deref r
    set r (f v)

-- | This constraint indicated that we can Union/Find values
--   `a` in the monad `r`
type UF r a = (Mem r, Val r ~ Node_ r a)

-- | A Union/Find Node `UF r a` actually stores references to a Node_ struct
--   which builds a tree of values
data Node_ r a = Node_ {
    parent :: Maybe (Ref r),
    rank :: Int,
    value :: a
}

-- | Mutable reference to a node in a Union/Find graph
newtype Node r = Node (Ref r)

-- | Create a new node
node :: UF r a => a -> r (Node r)
node a = do
    r <- ref (Node_ { parent = Nothing, rank = 0, value = a })
    return (Node r)

-- | Connect two nodes
link :: UF r a => Node r -> Node r -> r ()
link n1 n2 = do
    -- Find the two nodes
    Node p1 <- find n1
    Node p2 <- find n2

    unless (p1 == p2) (adopt p1 p2)

    where
        adopt x y = do
            -- Deref to access the rank of the nodes
            nx <- deref x
            ny <- deref y

            case compare (rank  nx) (rank ny) of
                -- If equal, increment the rank of the first node
                -- and change the parent of the second to the first
                EQ -> do set x (nx { rank = rank nx + 1 })
                         set y (ny { parent = Just x })
                LT -> undefined
                GT -> undefined

-- | Given two nodes, determine whether they are connected or not
connected :: UF r a => Node r -> Node r -> r Bool
connected n1 n2 = do
    Node p1 <- find n1
    Node p2 <- find n2
    -- Union/Find works by maintaining the invariant that two Nodes are in the
    -- same connected component iff their representative nodes are the same.
    return (p1 == p2)

-- | `find` takes any Node and returns another Node which is the
--   “representative” node for some connected component in the graph.
find :: UF r a => Node r -> r (Node r)
find (Node r) = do
    Node p <- findRec (Node r)

    -- *Path Compression*
    -- Don't rewrite the top root since it doesn't have parent
    unless (r == p) $ alter (\n -> n { parent = Just p }) r

    return (Node p)

    where
        -- | Recursively jump up `parent` links until we're
        --   at the root node
        findRec (Node r) = do
            n <- deref r
            case parent n of
                Nothing -> return (Node r)
                Just p -> find (Node p)

main = putStrLn "Hello, World"
