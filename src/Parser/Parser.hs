module Parser.Parser (parsing)
where

import Ast (
  Ast,
  Block (..),
  Expression (..),
  Identifier (..),
  Program (..),
  Statement (..),
 )
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Void (Void)
import Text.Megaparsec.Char (string)
import Text.Megaparsec (
  Parsec,
  anySingle,
  between,
  choice,
  manyTill,
  optional,
  parse,
  satisfy,
  sepBy,
  sepEndBy,
  withRecovery,
  (<|>),
  many,
  try,
 )

import Token (
  Token (..),
 )
import Eval.Object (ObjectType(StringLit))

type Parser = Parsec Void [Token]

parsing :: Ast
parsing ts = case parse parseProgram "" ts of
  Left err -> error $ show err
  Right program -> program

-- faz parse de um programa
parseProgram :: Parser Program
parseProgram = Program <$> manyTill parseStatementWithRecover (parseToken EOF)

parseStatementWithRecover :: Parser Statement
parseStatementWithRecover = withRecovery recover (parseStatement <* parseToken Semicolon)
 where
  recover = const $ IllegalStatement <$ manyTill anySingle (parseToken Semicolon)

-- faz parse de um statement
parseStatement :: Parser Statement
parseStatement =
  choice
    [ parseLet
    , ReturnStatement <$> parseReturn
    , ExpressionStatement <$> parseExpression
    , BlockStatement <$> parseBlockStatement
    ]

-- faz parse de um termo
parseTerm :: Parser Expression
parseTerm =
  choice
    [ IntLiteral . read <$> parseInteger
    , BooleanLiteral <$> parseBool
    , parseString
    , IdentifierExpression <$> (Identifier <$> parseIdentifier)
    , parens parseExpression
    , parseIf
    , parseFunction
    ]

-- faz parse de uma expressao
parseExpression :: Parser Expression
parseExpression = makeExprParser parseTerm table
 where
  table =
    [ -- function call

      [ postfix parseCall (flip CallExpression)
      ]
    , -- boolean negation and arithmetic negation

      [ prefix (parseToken Bang) NotExpression
      , prefix (parseToken Minus) NegateExpression
      ]
    , -- division and multiplication

      [ binary (parseToken Asterisk) MulExpression
      , binary (parseToken Slash) DivExpression
      ]
    , -- addition and subtraction

      [ binary (parseToken Plus) AddExpression
      , binary (parseToken Minus) SubExpression
      ]
    , -- comparison operators

      [ binary (parseToken Equal) EqualityExpression
      , binary (parseToken NotEqual) InequalityExpression
      , binary (parseToken LessThan) LessThanExpression
      , binary (parseToken GreaterThan) GreaterThanExpression
      ]
    , -- concatanation operator
      [ binary (parseToken Plus) ConcatExpression
      ]
    ]
  binary :: (Functor m) => m b -> (a -> a -> a) -> Operator m a
  binary name f = InfixL (f <$ name)

  prefix :: (Functor m) => m b -> (a -> a) -> Operator m a
  prefix name f = Prefix (f <$ name)

  postfix :: (Functor m) => m a1 -> (a1 -> a2 -> a2) -> Operator m a2
  postfix name f = Postfix (f <$> name)

-- faz parse de um bloco de statements
parseBlockStatement :: Parser Block
parseBlockStatement = Block <$> brackets (sepEndBy parseStatement (parseToken Semicolon))

-- faz parse de um token
parseToken :: Token -> Parser Token
parseToken t = satisfy (== t)

-- faz parse de um identificador
parseIdentifier :: Parser String
parseIdentifier = do
  result <- satisfy isIdentifier
  case result of
    (Ident i) -> return i
    _ -> fail "Expected Identifier"
 where
  isIdentifier (Ident _) = True
  isIdentifier _ = False

-- faz parse de um inteiro
parseInteger :: Parser String
parseInteger = do
  result <- satisfy isInteger
  case result of
    (Int int) -> return int
    _ -> fail "Expected Integer"
 where
  isInteger (Int _) = True
  isInteger _ = False

-- faz parse de um if
parseIf :: Parser Expression
parseIf = do
  cond <- parseToken If >> parseExpression
  thenBlock <- parseBlockStatement
  elseBlock <- optional $ parseToken Else >> parseBlockStatement
  return $ IfExpression cond thenBlock elseBlock

parseFunction :: Parser Expression
parseFunction = do
  params <- parseToken Function >> parens (sepBy (Identifier <$> parseIdentifier) (parseToken Comma))
  FunctionExpression params <$> parseBlockStatement

parseCall :: Parser [Expression]
parseCall = parens (sepBy parseExpression (parseToken Comma))

parseReturn :: Parser Expression
parseReturn = parseToken Return >> parseExpression

parseLet :: Parser Statement
parseLet = do
  name <- parseToken Let >> parseIdentifier
  expression <- parseToken Attrib >> parseExpression
  return $ LetStatement (Identifier name) expression

-- faz parse de um booleano
parseBool :: Parser Bool
parseBool = (True <$ parseToken TokTrue) <|> (False <$ parseToken TokFalse)

-- faz parse de algo entre colchetes
brackets :: Parser a -> Parser a
brackets = between (parseToken LBrace) (parseToken RBrace)

-- faz parse de algo entre parenteses
parens :: Parser a -> Parser a
parens = between (parseToken LParen) (parseToken RParen)


parseString :: Parser Expression
parseString =  do
  result <- satisfy isString
  case result of
    (Str str) -> return $ StringLiteral str
    _ -> fail "Expected StringLiteral"
 where
  isString (Str _) = True
  isString _ = False 