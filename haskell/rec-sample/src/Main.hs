{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Protolude
import Data.Functor.Foldable
import qualified Data.Text as T

-- | Tweets aren't like this at all, they're actually modeled exactly the other
-- way around and act more like a doubly-linked list, but hey, it makes for
-- a reasonable example.
data TweetF r
  = ThreadF Text r
  | EndF
  deriving (Show, Functor)

-- | This defines our fix point.
type Tweet = Fix TweetF

-- | In this case, `r` functions as recursive argument. But it can be so much
-- more!
thread :: Text -> Tweet -> Tweet
thread = (Fix .) . ThreadF

-- | You may also call it nil.
end :: Tweet
end = Fix EndF

-- | Mapping the text inside the tweet, using a catamorphism here. Notice that
-- we don't use any explicit recursion. It's completely abstracted away from us.
mapTweetText :: (Text -> Text) -> Tweet -> Tweet
mapTweetText f = cata alg
  where
    alg EndF = end
    alg (ThreadF t tw) = thread (f t) tw -- `r` is, again, the nested Tweet.

-- | Now this is exciting. We use `cata` again, but this time we have don't
-- transform the Tweet, but extract and fold a value out of it.
printTweet :: Tweet -> Text
printTweet = cata alg
  where
    alg EndF = ""
    -- It's now clear to see that `r` (or `tw` in this case) is the accumulated
    -- value. That's exactly the flexibility we gain from having TweetF as parameterized
    -- type. Also, once again the recursion is entirely implicit - and guaranteed
    -- to terminate!
    alg (ThreadF t tw) = T.unwords [t, "->", tw]

main :: IO ()
main = do
  let t = thread "I do(n)'t know how to use parentheses." (thread "I wish I could edit Tweets." end)
  putStrLn $ printTweet t
  -- Outputs: "I do(n)'t know how to use parentheses. -> I wish I could edit Tweets. ->"
  let t' = mapTweetText (T.replace "do(n)'t" "do(n't)") t
  putStrLn $ printTweet t'
  -- Outputs: "I do(n't) know how to use parentheses. -> I wish I could edit Tweets. ->"
