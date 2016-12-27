{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Protolude
import Data.Functor.Foldable
import qualified Data.Text as T

data TweetF r
  = ThreadF Text r
  | EndF
  deriving (Show, Functor)

type Tweet = Fix TweetF

thread :: Text -> Tweet -> Tweet
thread = (Fix .) . ThreadF

end :: Tweet
end = Fix EndF

mapTweetText :: (Text -> Text) -> Tweet -> Tweet
mapTweetText f = cata alg where
  alg EndF = end
  alg (ThreadF t tw) = thread (f t) tw

main :: IO ()
main = do
  let t = thread "I do(n)'t know how to use parentheses." (thread "I wish I could edit Tweets." end)
  let t' = mapTweetText (T.replace "do(n)'t" "do(n't)") t
  putStrLn @Text "Whatever."
