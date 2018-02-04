{-# LANGUAGE ScopedTypeVariables #-}
module Library where

import Prelude
import Data.Maybe (isNothing)

data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving Show

type IntTree = Tree Int

collapse :: Tree a -> [Maybe a]
collapse t =
    case t of
         Empty                -> [Nothing]
         Node a Empty   r     -> Just a : Nothing : collapse r
         Node a l       Empty -> Just a : collapse l ++ [Nothing]
         Node a l       r     -> Just a : collapse l ++ collapse r

expand :: [Maybe a] -> (Tree a, [Maybe a])
expand []        = (Empty, [])
expand [Nothing] = (Empty, [])
expand (Just a:l:r:as) | isNothing l && isNothing r = (Node a Empty Empty, as)
                       | isNothing l =
                            let (r', as') = expand (r:as)
                            in (Node a Empty r', as')
                       | otherwise =
                            let (l', as') = expand (l:r:as)
                                (r', as'') = expand as'
                            in (Node a l' r', as'')
expand as        = (Empty, as)
