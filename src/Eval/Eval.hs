module Eval.Eval where
import Ast (Program (..), Statement (..), Expression (..))
import Eval.Object (Object (..), ObjectType (..))


eval :: Program -> Object
eval (Program statements) = evalStatements statements

evalStatements :: [Statement] -> Object
evalStatements [] = Object Null
evalStatements (stmt : rest) = case stmt of
  ExpressionStatement expr -> evalExpression expr
  -- Handle other statement types if needed...
  _ -> evalStatements rest

evalExpression :: Expression -> Object
evalExpression expr = case expr of
  IntLiteral value -> Object (IntLit value)

