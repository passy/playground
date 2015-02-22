import Data.Monoid

data SL a = Empty | SL a :> a

instance Monoid (SL a) where
    mempty = Empty
    mappend xs Empty = xs
    mappend xs (ys :> y) = mappend xs ys :> y

main :: IO ()
main = do
    putStrLn "hello world"
    -- TODO: And now?
