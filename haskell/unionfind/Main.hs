{-# LANGUAGE TypeFamilies, FlexibleContexts, ConstraintKinds #-}

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
type UF r a = ...

-- | Mutable reference to a node in a Union/Find graph
newtype Node r = Node (Ref r)

-- | Create a new node
node :: UF r a => a -> r (Node r)
node = undefined

-- | Connect two nodes
link :: UF r a => Node r -> Node r -> r ()
link = undefined

-- | Given two nodes, determine whether they are connected or not
connected :: UF r a => Node r -> Node r -> r Bool
connected = undefined

main = putStrLn "Hello, World"
