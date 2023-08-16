module Main (main) where

import Ast (Program)
import Eval.Eval (eval)
import Eval.Object (inspect)
import Lexer.Lexer (tokenize)
import Parser.Parser (parsing)
import System.IO (hFlush, stdout)

prompt :: String -> IO String
prompt what = do
  putStr what
  hFlush stdout
  getLine

interpret = inspect . eval . parsing . tokenize
main :: IO ()
main = do
  expression <- prompt ">> "
  print $ interpret expression
