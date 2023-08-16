{-# LANGUAGE RecordWildCards #-}

module Eval.Object where

import Ast (Program)

type Eval = Program -> String

data ObjectType
  = IntLit Integer
  | Boolean Bool
  | Null
  deriving (Eq, Show)

newtype Object = Object
  { objectType :: ObjectType
  }
  deriving (Eq, Show)

inspect :: Object -> String
inspect Object{..} =
  case objectType of
    IntLit i -> show i
    Boolean b -> show b
    Null -> "null"
    otherwise -> "unknown"
