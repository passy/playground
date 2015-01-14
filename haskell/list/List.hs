data List a = End | Cons a (List a)
    deriving (Show)

instance Functor List where
    fmap _ End         = End
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

main :: IO ()
main = do
    let l = Cons 2 (Cons 3 (Cons 5 (Cons 8 End))) :: List Int
    print l
    print $ fmap (*2) l
