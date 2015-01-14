{-# Language KindSignatures #-}

data List a = End | Cons a (List a)
    deriving (Show)

instance Functor List where
    fmap _ End         = End
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)


class Functor f => Applicative (f :: * -> *) where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

instance Applicative List where
    pure a = Cons a End
    (<*>) = undefined

main :: IO ()
main = do
    let l = Cons 2 (Cons 3 (Cons 5 (Cons 8 End))) :: List Int
    print l
    print $ fmap (*2) l
