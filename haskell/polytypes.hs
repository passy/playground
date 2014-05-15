{-# LANGUAGE DataKinds,DeriveDataTypeable #-}

import Control.Exception
import Data.Typeable

data ErrorCatA = WTFError String | OMGError String
    deriving (Typeable, Show)

data ErrorCatB = BadError Int | TerribleError Float
    deriving (Typeable, Show)

instance Exception ErrorCatA
instance Exception ErrorCatB

handleError :: SomeException -> IO ()
handleError err =
    putStrLn $ case fromException err of
        Just (WTFError s) -> "WTF?! " ++ s
        Just (OMGError s) -> "OMG?! " ++ s
        _ -> case fromException err of
            Just (BadError i) -> if i > 5 then "Really bad" else "Not that bad"
            Just (TerribleError f) -> "Terrible! " ++ show f
            _ -> "No idea"

main :: IO ()
main = do
    let err0 = toException $ WTFError "Sky's falling."
    let err1 = toException $ TerribleError 1.337

    handleError err0
    handleError err1

    handleError $ toException $ WTFError "Yo"
    handleError $ toException $ OMGError "this can't be right ..."
