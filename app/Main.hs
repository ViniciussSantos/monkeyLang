module Main (main) where

import Lexer.Lexer (tokenize)
import System.IO (hFlush, stdout)

prompt :: String -> IO String
prompt what = do
  putStr what
  hFlush stdout
  getLine

main :: IO ()
main = do
  expression <- prompt ">> "
  print (tokenize expression)
