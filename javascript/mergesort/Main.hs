module Main where

mergesort :: [Int] -> [Int]
mergesort [] = []
mergesort [x] = [x]
mergesort xs =
  let
    s = length xs `div` 2
    first = mergesort $ take s xs
    last' = mergesort $ drop s xs
  in
    merge first last'

merge :: [Int] -> [Int] -> [Int]
merge as     []     = as
merge []     bs     = bs
merge (a:as) (b:bs) = if a <= b then a : merge as (b:bs) else b : merge (a:as) bs

main :: IO ()
main =
  let arr = [5, 2, 6, 3, 1, 7, 0, 10, 12, 8, 5]
  in print $ mergesort arr
