{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (void)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Text as M
import qualified System.Environment as Env

type Tag = (String, String)

tagP :: M.Parser Tag
tagP = do
  void $ M.char '#'
  key <- M.someTill M.printChar (M.char ':')
  val <- M.someTill M.printChar M.eol
  return (key, val)

parser :: M.Parser [Tag]
parser = M.many tagP

main :: IO ()
main = do
  fname <- head <$> Env.getArgs
  res <- M.parseFromFile (parser <* M.eof) fname
  print res
