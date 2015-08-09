#!/usr/bin/env stack exec runhaskell
{-# LANGUAGE DeriveFunctor #-}

data Tree a = Node a (Tree a) (Tree a) | Empty
  deriving (Show, Functor)

traverseDF :: Tree a -> [a]
traverseDF Empty = []
traverseDF (Node v l r) =
  [v] ++ traverseDF l ++ traverseDF r

main :: IO ()
main = do
  let t = Node "A" (Node "B" (Node "C" Empty Empty) Empty) (Node "D" Empty (Node "E" (Node "F" Empty Empty) Empty))
  print $ traverseDF t
