{-# Language KindSignatures, NoImplicitPrelude #-}

import Prelude hiding (Applicative, (<*>), pure, zipWith)

data List a = Nil | Cons a (List a)
    deriving (Show)

instance Functor List where
    fmap _ Nil         = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

class Functor f => Applicative (f :: * -> *) where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

instance Applicative List where
    pure a = Cons a Nil
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    (Cons f fs) <*> x = append (fmap f x) (fs <*> x)

zipWith :: (a -> b -> c) -> List a -> List b -> List c
zipWith f (Cons a as) (Cons b bs) = Cons (f a b) (zipWith f as bs)
zipWith _ _ _ = Nil

append :: List a -> List a -> List a
append Nil a = a
append (Cons a as) b = Cons a (append as b)

main :: IO ()
main = do
    let l = Cons 2 (Cons 3 (Cons 5 (Cons 8 Nil))) :: List Int
    print l
    print $ zipWith (+) l l
    print $ fmap (*2) l
    print $ pure (*2) <*> l
    print $ (Cons (*3) (Cons (^2) Nil)) <*> l
