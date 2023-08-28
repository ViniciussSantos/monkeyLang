module Eval.Eval where

import Ast (Block (..), Expression (..), Program (..), Statement (..))
import Eval.Object (Object (..), ObjectType (..))

eval :: Program -> Object
eval (Program statements) = evalStatements statements

evalStatements :: [Statement] -> Object
evalStatements [] = Object Null
evalStatements (stmt : rest) = case stmt of
  ExpressionStatement expr -> evalExpression expr
  BlockStatement (Block stmts) -> evalStatements stmts
  -- Handle other statement types if needed...
  _ -> evalStatements rest

evalBlock :: Block -> Object
evalBlock (Block stmts) = evalStatements stmts

evalPrefixExpression :: String -> Object -> Object
evalPrefixExpression operator right = case right of
  Object (IntLit right') -> case operator of
    "-" -> Object (IntLit (-right'))
    _ -> Object Null
  Object (Boolean right') -> case operator of
    "!" -> Object (Boolean (not right'))
    _ -> Object Null
  _ -> Object Null

evalInfixExpression :: String -> Object -> Object -> Object
evalInfixExpression operator left right = case (left, right) of
  (Object (IntLit left'), Object (IntLit right')) -> case operator of
    "+" -> Object (IntLit (left' + right'))
    "-" -> Object (IntLit (left' - right'))
    "*" -> Object (IntLit (left' * right'))
    "/" -> Object (IntLit (left' `div` right'))
    "<" -> Object (Boolean (left' < right'))
    ">" -> Object (Boolean (left' > right'))
    "==" -> Object (Boolean (left' == right'))
    "!=" -> Object (Boolean (left' /= right'))
    _ -> Object Null
  (Object (Boolean left'), Object (Boolean right')) -> case operator of
    "==" -> Object (Boolean (left' == right'))
    "!=" -> Object (Boolean (left' /= right'))
    _ -> Object Null
  _ -> Object Null

evalExpression :: Expression -> Object
evalExpression expr = case expr of
  IntLiteral value -> Object (IntLit value)
  BooleanLiteral value -> Object (Boolean value)
  NotExpression e -> evalPrefixExpression "!" (evalExpression e)
  NegateExpression e -> evalPrefixExpression "-" (evalExpression e)
  AddExpression left right -> evalInfixExpression "+" (evalExpression left) (evalExpression right)
  SubExpression left right -> evalInfixExpression "-" (evalExpression left) (evalExpression right)
  MulExpression left right -> evalInfixExpression "*" (evalExpression left) (evalExpression right)
  DivExpression left right -> evalInfixExpression "/" (evalExpression left) (evalExpression right)
  EqualityExpression left right -> evalInfixExpression "==" (evalExpression left) (evalExpression right)
  InequalityExpression left right -> evalInfixExpression "!=" (evalExpression left) (evalExpression right)
  LessThanExpression left right -> evalInfixExpression "<" (evalExpression left) (evalExpression right)
  GreaterThanExpression left right -> evalInfixExpression ">" (evalExpression left) (evalExpression right)
  IfExpression e block Nothing -> case evalExpression e of
    Object (Boolean True) -> evalBlock block
    _ -> Object Null
  IfExpression e block (Just (Block [])) -> case evalExpression e of
    Object (Boolean True) -> evalBlock block
    _ -> Object Null
  IfExpression e block (Just (Block stmts)) -> case evalExpression e of
    Object (Boolean True) -> evalBlock block
    _ -> evalStatements stmts
