{-# LANGUAGE RecordWildCards #-}

module Eval.Object where

import Data.Char (toLower)

data ObjectType
  = IntLit Integer
  | Boolean Bool
  | StringLit String
  | Null
  deriving (Eq)

newtype Object = Object
  { objectType :: ObjectType
  }
  deriving (Eq)

instance Show ObjectType where
  show (IntLit i) = show i
  show (Boolean b) = map toLower (show b)
  show (StringLit s) = removeEscapeChars s
  show Null = "null"

instance Show Object where
  show (Object objType) = show objType


-- Remove escape chars
removeEscapeChars :: String -> String
removeEscapeChars [] = []
removeEscapeChars ('\\' : 'n' : xs) = '\n' : removeEscapeChars xs
removeEscapeChars ('\\' : 'r' : xs) = '\r' : removeEscapeChars xs
removeEscapeChars ('\\' : 't' : xs) = '\t' : removeEscapeChars xs
removeEscapeChars ('\\' : '\\' : xs) = '\\' : removeEscapeChars xs
removeEscapeChars ('\\' : '"' : xs) = '"' : removeEscapeChars xs
removeEscapeChars (x : xs) = x : removeEscapeChars xs
