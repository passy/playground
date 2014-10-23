-- This is for @monchote

import Prelude hiding (mapM_)
import Data.Foldable (mapM_)
import System.Random (getStdRandom, randomR)

type UserId = Int

data User = User { userName :: String
                 , userId :: UserId
                 } deriving (Show, Eq)

flipCoin :: IO Int
flipCoin = getStdRandom (randomR (0, 1))

getUser :: UserId -> IO (Maybe User)
getUser _ = do
    r <- flipCoin
    return $ if r == 0
        then Just $ User "monchote" 210789
        else Nothing

printUserName :: User -> IO ()
printUserName = putStrLn . userName

main :: IO ()
main = do
    u <- getUser 12345
    mapM_ printUserName u

    -- Alternatively:
    maybe (return ()) print u

    -- Or very explicitly:
    case u of
        Just user -> printUserName user
        Nothing   -> return ()
