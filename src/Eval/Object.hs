{-# LANGUAGE RecordWildCards #-}

module Eval.Object where

import Data.Char (toLower)

data ObjectType
  = IntLit Integer
  | Boolean Bool
  | Null
  deriving (Eq)

newtype Object = Object
  { objectType :: ObjectType
  }
  deriving (Eq)

instance Show ObjectType where
  show (IntLit i) = show i
  show (Boolean b) = map toLower (show b)
  show Null = "null"

instance Show Object where
  show (Object objType) = show objType
