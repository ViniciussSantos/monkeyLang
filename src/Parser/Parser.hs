{-# LANGUAGE ImportQualifiedPost #-}

module Parser.Parser (
  )
where

import Ast
import Data.Void (Void)
import Text.Megaparsec
import Token
import Token qualified as T

type Parser = Parsec Void [T.Token]

-- faz parse de um programa
parseProgram :: Parser Program
parseProgram = undefined

-- faz parse de um statement
parseStatement :: Parser Program
parseStatement = undefined

-- faz parse de uma expressao
parseExpression :: Parser Expression
parseExpression = undefined

-- faz parse de um identificador
parseIdentifier :: Parser String
parseIdentifier = do
  result <- satisfy isIdentifier
  case result of
    (T.Ident t) -> return t
    _ -> fail "Expected Identifier"
 where
  isIdentifier (T.Ident _) = True
  isIdentifier _ = False

-- faz parse de um token
parseToken :: T.Token -> Parser T.Token
parseToken t = satisfy (== t)

-- faz parse de um inteiro
parseInteger :: Parser String
parseInteger = do
  result <- satisfy isInteger
  case result of
    (Int t) -> return t
    _ -> fail "Expected Integer"
 where
  isInteger (Int _) = True
  isInteger _ = False
