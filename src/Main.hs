{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified System.Environment   as Env
import qualified Text.Megaparsec      as M
import qualified Text.Megaparsec.Text as M

type Tag = (String, String)

tagP :: M.Parser Tag
tagP = M.char '#' *> ((,) <$> M.someTill M.printChar (M.char ':') <*> M.someTill M.printChar M.eol) M.<?> "Tag starting with #"

urlP :: M.Parser String
urlP = M.someTill M.printChar M.eol M.<?> "Some URL"

data Segment = Tags [Tag] | URL String
  deriving (Eq, Show)

parser :: M.Parser Segment
parser = (Tags <$> M.many tagP) M.<|> (URL <$> urlP)

main :: IO ()
main = do
  fname <- head <$> Env.getArgs
  res <- M.parseFromFile (parser <* M.eof) fname
  print res
