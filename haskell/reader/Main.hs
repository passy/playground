newtype Reader r a = Reader { runReader :: r -> a }

instance Monad (Reader r) where
    return a = Reader $ \_ -> a
    m >>= k = Reader $ \r -> runReader (k $ runReader m r) r

asks :: (r -> a) -> Reader r a
asks f = Reader f

ask :: Reader a a
ask = Reader id

-- Example Code

data MyContext = MyContext {
    foo :: String,
    bar :: Int
} deriving (Show)

computation :: Reader MyContext (Maybe String)
computation = do
    n <- asks bar
    x <- asks foo
    if n > 0
        then return (Just x)
        else return Nothing

ex1 :: Maybe String
ex1 = runReader computation $ MyContext "hello" 1

ex2 :: Maybe String
ex2 = runReader computation $ MyContext "haskell" 0

ex3 :: String
ex3 = runReader computation2 "Haskell"
    where
        computation2 :: Reader String String
        computation2 = do
            name <- ask
            return $ "Hello, " ++ name

main :: IO ()
main = do
    print ex1
    print ex2
    print ex3
