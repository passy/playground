{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Text as M
import qualified System.Environment as Env

type Tag = (String, String)

tagP :: M.Parser Tag
tagP = M.char '#' *> ((,) <$> M.someTill M.printChar (M.char ':') <*> M.someTill M.printChar M.eol)

parser :: M.Parser [Tag]
parser = M.many tagP

main :: IO ()
main = do
  fname <- head <$> Env.getArgs
  res <- M.parseFromFile (parser <* M.eof) fname
  print res
