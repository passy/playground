data List a = End | Cons a (List a)
    deriving (Show)


main :: IO ()
main = do
    let l = Cons 2 (Cons 3 (Cons 5 (Cons 8 End))) :: List Int
    print l
