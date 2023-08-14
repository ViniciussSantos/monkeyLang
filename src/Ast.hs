module Ast (
  Ast,
  Program,
  Statement (..),
  Expression (..),
  Identifier (..),
) where

import Token (Token)

type Ast = [Token] -> Program

newtype Program = Program [Statement]
  deriving (Eq, Show)

data Statement
  = LetStatement Identifier Expression
  | ReturnStatement Expression
  | ExpressionStatement Expression
  | BlockStatement [Statement]
  | IllegalStatement
  deriving (Eq, Show)

data Expression
  = IntLiteral Integer
  | BooleanLiteral Bool
  | IdentifierExpression Identifier
  | PrefixExpression Token Expression
  | InfixExpression Token Expression Expression
  | IfExpression Expression [Statement] (Maybe [Statement])
  | FunctionExpression [Identifier] [Statement]
  | CallExpression Expression [Expression]
  | IllegalExpression
  deriving (Eq, Show)

newtype Identifier = Identifier String
  deriving (Eq, Show)