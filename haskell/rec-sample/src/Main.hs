{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Protolude
import Data.Functor.Foldable
import qualified Data.Text as T

data TweetF r
  = ThreadF Text r
  | EndF Text
  deriving (Show, Functor)

type Tweet = Fix TweetF

thread :: Text -> Tweet -> Tweet
thread = (Fix .) . ThreadF

end :: Text -> Tweet
end = Fix . EndF

replaceTweet :: Text -> Text -> Tweet -> Tweet
replaceTweet needle replacement = cata alg where
  replace = T.replace needle replacement
  alg (EndF t) = end $ replace t
  alg (ThreadF t tw) = thread (replace t) tw

main :: IO ()
main = do
  let t = thread "I do(n)'t know how to use parentheses." (end "I wish I could edit Tweets.")
  let t' = replaceTweet "do(n)'t" "do(n't)" t
  putStrLn @Text "Whatever."
