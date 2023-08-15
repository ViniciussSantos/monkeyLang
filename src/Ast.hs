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
  | BlockStatement Block
  | IllegalStatement
  deriving (Eq, Show)

data Expression
  = IntLiteral Integer
  | BooleanLiteral Bool
  | IdentifierExpression Identifier
  | AddExpression Expression Expression
  | SubExpression Expression Expression
  | MulExpression Expression Expression
  | DivExpression Expression Expression
  | EqualityExpression Expression Expression
  | InequalityExpression Expression Expression
  | LessThanExpression Expression Expression
  | GreaterThanExpression Expression Expression
  | NotExpression Expression
  | NegateExpression Expression
  | IfExpression Expression Block (Maybe Block)
  | FunctionExpression [Identifier] Block
  | CallExpression Expression [Expression]
  deriving (Eq, Show)

newtype Block = Block [Statement]
  deriving (Eq, Show)

newtype Identifier = Identifier String
  deriving (Eq, Show)
