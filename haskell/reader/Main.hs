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
    name :: String,
    age :: Int
}

bartender :: Reader MyContext (Maybe String)
bartender = do
    n <- asks name
    a <- asks age
    return $ if a >= 18
        then (Just n)
        else Nothing

pub :: IO ()
pub =
    putStrLn $ case computation of
        (Just n) -> "Have a lager, " ++ n
        Nothing -> "No drinks for you"
    where
        computation :: Maybe String
        computation = runReader bartender $ MyContext "@phuunet" 16

-- DI Example
data DIContext = DIContext {
    prettyPrint :: String -> Int -> String
}

main :: IO ()
main = pub
