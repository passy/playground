{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative  (liftA2)
import qualified Data.Text.IO         as TIO
import qualified System.Environment   as Env
import qualified Text.Megaparsec      as M
import qualified Text.Megaparsec.Text as M

type Tag = (String, String)

data Segment = Tags [Tag] | URL String
  deriving (Eq, Show)

tagP :: M.Parser Tag
tagP = M.char '#' *> ((,) <$> M.someTill M.printChar (M.char ':') <*> M.someTill M.printChar M.eol) M.<?> "Tag starting with #"

urlP :: M.Parser String
urlP = M.someTill M.printChar M.eol M.<?> "Some URL"

parser :: M.Parser ([Tag], String)
parser = liftA2 (,) (M.many tagP) urlP

main :: IO ()
main = do
  fname <- head <$> Env.getArgs
  res <- M.parse (M.many parser <* M.eof) mempty <$> TIO.readFile fname
  case res of
    Left err -> putStrLn $ M.parseErrorPretty err
    Right res' -> print res'
