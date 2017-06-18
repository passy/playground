import Data.Map.Strict

countInstances :: [String] -> Map String Int
countInstances = foldl (\acc i -> insertWith (+) i 1 acc) mempty

main :: IO ()
main = do
    print $ countInstances ["hello", "hello", "world"]
