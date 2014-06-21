{-# Language NoImplicitPrelude, OverloadedStrings, TemplateHaskell #-}

import Prelude
import Control.Lens

data RequestMethod = GET | POST
    deriving (Show, Eq)

data Request = Request {
    _host :: String,
    _headers :: [(String, String)],
    _method :: RequestMethod
} deriving (Show, Eq)

makeLenses ''Request

mkRequest :: RequestMethod -> String -> Request
mkRequest method' host' = Request host' [] method'

addHeader :: Request -> String -> String -> Request
addHeader req key value =
    set headers ((req ^. headers) ++ [(key, value)]) req

main :: IO ()
main = do
    let req = mkRequest GET "phuu.net"
    print req
    let req' = addHeader req "Accept" "txt/bullshit"
    print req'
