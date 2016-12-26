module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

data Natural =
    Zero
  | Succ Natural

data NatF r =
    ZeroF
  | SuccF r

instance natFFunctor :: Functor NatF where
  map f ZeroF = ZeroF
  map f (SuccF r) = SuccF $ f r

newtype Fix f = Fix (f (Fix f))

unfix :: forall f. Fix f -> f (Fix f)
unfix (Fix f) = f

type Nat = Fix NatF

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"
