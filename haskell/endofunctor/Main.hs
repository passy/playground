{-# LANGUAGE RankNTypes, TypeOperators #-}

import Control.Monad (join)

newtype f :-> g = Natural { eta :: forall x. f x -> g x }

listToMaybe :: [] :-> Maybe
listToMaybe = Natural go
    where
        go [] = Nothing
        go (x:_) = Just x

maybeToList :: Maybe :-> []
maybeToList = Natural go
    where
        go (Just x) = [x]
        go Nothing = []

reverse' :: [] :-> []
reverse' = Natural reverse

newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

return' :: Monad t => Identity :-> t
return' = Natural (return . runIdentity)

newtype Compose f g a = Compose { getCompose :: f (g a) }

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose fga) = Compose (fmap (fmap f) fga)

join' :: Monad t => Compose t t :-> t
join' = Natural (join . getCompose)

main :: IO ()
main = return ()
