module Main where

quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort [x] = [x]
quicksort (x:xs) =
  let smaller = quicksort [a | a <- xs, a <= x]
      bigger  = quicksort [a | a <- xs, a > x]
  in smaller ++ pure x ++ bigger

main :: IO ()
main = print $ quicksort [5, 2, 6, 3, 1, 7, 0, 10, 12, 8, 5]
