{-# LANGUAGE DeriveFunctor #-}

import Control.Applicative

newtype State s a = State { runState :: s -> (a, s) }
    deriving (Functor)

instance Applicative (State s) where
    pure a = State $ \s -> (a, s)
    (State s1) <*> (State s2) = State $ \s -> s1

instance Monad (State s) where
    return = pure
    m >>= k = State $ \s -> let
        (a', s') = runState m s
        in runState (k a') s'

state :: (s -> (a, s)) -> State s a
state = State

get :: State s s
get = State $ \st -> (st, st)

put :: s -> State s ()
put n = State $ \_ -> ((), n)

main :: IO ()
main = print $ runState myState 1
    where
        myState = do
            x <- get
            put (x + 1)
