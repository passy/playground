module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

-- | Our usual Fix definition.
newtype Fix f = Fix (f (Fix f))

-- | Peel back one layer of recursion.
unFix :: forall f. Fix f -> f (Fix f)
unFix (Fix f) = f

-- | A simpler definition of a catamorphism, without genericizing this
-- to Base as it's done in `recursion-schemes`.
cata :: forall f a. Functor f => (f a -> a) -> Fix f -> a
cata alg = unFix >>> map (cata alg) >>> alg

-- Point-ful version.
cata' :: forall f a. Functor f => (f a -> a) -> Fix f -> a
cata' alg c = alg (map (cata alg) (unFix c))

-- | A pattern functor for a cons-list.
data ListF a b = Nil | Cons a b

-- | We could derive this (now even in PureScript!) but it's simple
-- enough that we can write it out to remind ourselves later
-- about how the recursive step works.
instance functorListF :: Functor (ListF a) where
  map f Nil = Nil
  map f (Cons e x) = Cons e $ f x

-- | Save us a bit of typing by wrappping ListF and the constructors
-- with their fix points.
type List a = Fix (ListF a)

cons :: forall a. a -> List a -> List a
cons a b = Fix $ Cons a b

nil :: forall a. List a
nil = Fix Nil

-- | Our sample list.
lst :: List Int
lst = cons 2 (cons 3 nil)

-- | An algebra for summing up a list of integers.
algSum :: ListF Int Int -> Int
algSum Nil = 0
algSum (Cons n acc) = n + acc

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  let res = cata algSum (Fix Nil)
  --   cata algSum (Fix Nil)
  -- = algSum (map (cata algSum) (unFix (Fix Nil)))
  --                              ^^^^^^^^^^^^^^^
  --                      forall a. ListF a (Fix (ListF a))
  -- = algSum (map (cata algSum) (unFix (Fix Nil)))
  --           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  --                     ListF Int Int
  -- = algSum Nil
  -- = 0
  --
  -- map undefined Nil = Nil
  log <<< show $ res
  log <<< show $ cata algSum lst
