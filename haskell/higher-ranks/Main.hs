{-# LANGUAGE ExistentialQuantification #-}


main :: IO ()
main =
    print $ fmap f showables
      where f (MkShowable a) = show a

data Showable = forall a. Show a => MkShowable a

pack :: Show a => a -> Showable
pack = MkShowable

showables :: [Showable]
showables = [pack 1, pack "hi", pack 25.0]
