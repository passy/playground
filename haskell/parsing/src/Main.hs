{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified System.Environment   as Env
import qualified Text.Megaparsec      as M
import qualified Text.Megaparsec.Text as M
import qualified Data.Text.IO         as TIO

type Tag = (String, String)

data Segment = Tags [Tag] | URL String
  deriving (Eq, Show)

tagP :: M.Parser Tag
tagP = M.char '#' *> ((,) <$> M.someTill M.printChar (M.char ':') <*> M.someTill M.printChar M.eol) M.<?> "Tag starting with #"

urlP :: M.Parser String
urlP = M.someTill M.printChar M.eol M.<?> "Some URL"

parser :: M.Parser Segment
parser = (Tags <$> M.many tagP) M.<|> (URL <$> urlP)

main :: IO ()
main = do
  fname <- head <$> Env.getArgs
  res <- M.parse (parser <* M.eof) mempty <$> TIO.readFile fname
  case res of
    Left err -> putStrLn $ M.parseErrorPretty err
    Right res' -> print res'
