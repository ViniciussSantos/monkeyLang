module Main (main) where

import Eval.Eval (eval)
import Lexer.Lexer (tokenize)
import Parser.Parser (parsing)
import System.IO (hFlush, stdout)
import Eval.Object (Object)

prompt :: String -> IO String
prompt what = do
  putStr what
  hFlush stdout
  getLine

interpret :: String -> String
interpret = show . eval . parsing . tokenize

main :: IO ()
main = do
  expression <- prompt ">> "
  print $ interpret expression
