module Lexer.Token (
  Token (..),
  isIdentifierChar,
  identifierToken,
)
where

import Data.Char (isLetter)


data Token
  = Illegal -- Token Invalido
  | EOF -- Fim do Arquivo
  | Ident String -- Indetificadores
  | Int String -- Numeros inteiros
  | Attrib -- Operador de atribuicao
  | Plus -- Operador de adicao
  | Minus -- Operador de subtracao
  | Bang -- Operador de negacao
  | Asterisk -- Operador de multiplicacao
  | Slash -- Operador de divisao
  | LessThan -- Operador de menor
  | GreaterThan -- Operador de maior
  | TokTrue -- Boolean true
  | TokFalse -- Boolean false
  | Equal -- Operador de igualdade
  | NotEqual -- Operador de desigualdade
  | Comma -- Operador de virgula
  | Semicolon -- Operador de ponto e virgula
  | LParen -- Parentese esquerdo
  | RParen -- Parentese direito
  | LBrace -- Chave esquerdo
  | RBrace -- Chave direito
  | Function -- Palavra-chave funcion
  | Let -- Palavra-chave let
  | If -- Palavra-chave if
  | Else -- Palavra-chave else
  | Return -- Palavra-chave return
  deriving (Show, Eq, Ord)

-- Verifica se é um caractere de identificador
-- _ pq podemos ter identificadores com _ no nome
isIdentifierChar :: Char -> Bool
isIdentifierChar char = isLetter char || char == '_'

-- Verifica token se um token é um identificador ou uma keyword
identifierToken :: String -> Token
identifierToken "true" = TokTrue
identifierToken "false" = TokFalse
identifierToken "fn" = Function
identifierToken "let" = Let
identifierToken "if" = If
identifierToken "else" = Else
identifierToken "return" = Return
identifierToken x = Ident x
