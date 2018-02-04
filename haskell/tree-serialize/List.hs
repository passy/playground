{-# LANGUAGE ScopedTypeVariables #-}
import Prelude

data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving Show

type IntTree = Tree Int

collapse :: IntTree -> [Int]
collapse t =
    case t of
         Empty                -> [-1]
         Node a Empty   r     -> a : -1 : collapse r
         Node a l       Empty -> a : collapse l ++ [-1]
         Node a l       r     -> a : collapse l ++ collapse r

expand :: [Int] -> (IntTree, [Int])
expand []   = (Empty, [])
expand [-1] = (Empty, [])
expand (a:l:r:as) | l == -1 && r == -1 = (Node a Empty Empty, as)
                  | l == -1 =
                    let (r', as') = expand (r:as)
                    in (Node a Empty r', as')
                  | otherwise =
                    let (l', as') = expand (l:r:as)
                        (r', as'') = expand as'
                    in (Node a l' r', as'')

main = 
    --         1
    --          \
    --           2
    --         /   \
    --        3     4
    --               \
    --                5
    --
    let tree :: Tree Int = Node 1 Empty (Node 2 (Node 3 Empty Empty) (Node 4 Empty (Node 5 Empty Empty)))
        c = collapse tree
        e = expand c
    in do
        putStrLn $ "Collapsed: " ++ show c
        putStrLn $ "Expanded: " ++ show e
