{-# Language NoImplicitPrelude, OverloadedStrings #-}

import Prelude

data RequestMethod = GET | POST
    deriving (Show, Eq)

data Request = Request {
    host :: String,
    headers :: [(String, String)],
    method :: RequestMethod
} deriving (Show, Eq)

mkRequest :: RequestMethod -> String -> Request
mkRequest method' host' = Request host' [] method'

addHeader :: Request -> String -> String -> Request
addHeader req key value =
    Request {
        host = host req,
        headers = headers req ++ [(key, value)],
        method = method req
    }

main :: IO ()
main = do
    let req = mkRequest GET "phuu.net"
    print req
    let req' = addHeader req "Accept" "txt/bullshit"
    print req'
