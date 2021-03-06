{-# Language DeriveFunctor, GeneralizedNewtypeDeriving #-}
import Control.Applicative

-- Holy fuck, how is this even possible to derive?
newtype Reader r a = Reader { runReader :: r -> a }
    deriving (Functor, Applicative)

instance Monad (Reader r) where
    return = pure
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

--

example2 :: String -> String
example2 context = runReader (computation "Tom") context
    where
        computation :: String -> Reader String String
        computation name = do
            greeting <- ask
            return $ greeting ++ ", " ++ name


example3 :: String -> String
example3 context = runReader (greet "James" >>= end) context
    where
        greet :: String -> Reader String String
        greet name = do
            greeting <- ask
            return $ greeting ++ ", " ++ name

        end :: String -> Reader String String
        end input = do
            isHello <- asks (== "Hello")
            return $ input ++ if isHello then "!" else "."

example4 :: String -> String
example4 context = runReader (greet "James" >>= end) context
    where
        greet :: String -> Reader String String
        greet name = do
            greeting <- ask
            return $ greeting ++ ", " ++ name

        end :: String -> Reader String String
        end input = do
            isHello <- asks (== "Hello")
            hasE <- asks((== "e") . (drop 1) . (take 2))
            let count = if hasE then 3 else 1
            return $ input ++ (concat $ replicate count $ if isHello then "!" else ".")

        end' :: String -> Reader String String
        end' input =
            asks (== "Hello") >>= (\isHello ->
                asks((== "e") . (drop 1) . (take 2)) >>= (\hasE ->
                    let count = if hasE then 3 else 1 in
                    return $ input ++ (concat $ replicate count $ if isHello then "!" else ".")
                )
            )

main :: IO ()
main = putStrLn $ example4 "Hi"

-- DI Example
data DIContext = DIContext {
    prettyPrint :: String -> Int -> String
}

-- Silly example

silly :: Reader [a] Int
silly = asks length

mainSilly :: IO ()
mainSilly =
    print $ runReader silly [1, 2, 3]
