module Main where

import Library

main = 
    --         1
    --          \
    --           2
    --         /   \
    --        3     4
    --               \
    --                5
    --
    let tree = Node 1 Empty (Node 2 (Node 3 Empty Empty) (Node 4 Empty (Node 5 Empty Empty)))
        c = collapse tree
        e = expand c
    in do
        putStrLn $ "Collapsed: " ++ show c
        putStrLn $ "Expanded: " ++ show e
