{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}

module Lexer.Lexer (
  tokenize,
  newLexer,
  nextToken,
  Lexer (..),
  Input,
) where

import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.Char (isDigit, isSpace)
import Data.Maybe (fromMaybe, isJust)
import Lexer.Token (Token (..), identifierToken, isIdentifierChar)

-- usado por performance
type Input = ByteString

-- record do Lexer
data Lexer = Lexer
  { input :: Input -- Entrada
  , position :: Int -- Posicao atual do input (o caractere atual)
  , readPosition :: Int -- posição do próximo caractere a ser lido
  , currentChar :: Char -- Caractere atual sendo examinado
  }

-- Tokeniza a entrada
tokenize :: String -> [Token]
tokenize = go . advance . newLexer . B.pack
 where
  go lexer = case nextToken lexer of
    (EOF, _) -> [EOF]
    (token, lexer') -> token : go lexer'

-- Cria um novo lexer a partir de uma entrada
newLexer :: Input -> Lexer
newLexer input =
  Lexer
    { input = input
    , position = 0
    , readPosition = 0
    , currentChar = '\0'
    }

-- Retorna o próximo token e o lexer atualizado
nextToken :: Lexer -> (Token, Lexer)
nextToken lexer = (token, lexer'')
 where
  lexer' = skipSpace lexer
  (token, lexer'') =
    case currentChar lexer' of
      '{' -> (LBrace, advance lexer')
      '}' -> (RBrace, advance lexer')
      '(' -> (LParen, advance lexer')
      ')' -> (RParen, advance lexer')
      ',' -> (Comma, advance lexer')
      ';' -> (Semicolon, advance lexer')
      '+' -> (Plus, advance lexer')
      '-' -> (Minus, advance lexer')
      '!' ->
        if isJust (peek lexer')
          && peek lexer' == Just '='
          then (NotEqual, advance $ advance lexer')
          else (Bang, advance lexer')
      '>' -> (GreaterThan, advance lexer')
      '<' -> (LessThan, advance lexer')
      '*' -> (Asterisk, advance lexer')
      '/' -> (Slash, advance lexer')
      '=' ->
        if isJust (peek lexer')
          && peek lexer' == Just '='
          then (Equal, advance $ advance lexer')
          else (Attrib, advance lexer')
      '\0' -> (EOF, lexer')
      c
        | isIdentifierChar c ->
            let (str, lexer''') = readIdentifier lexer'
             in (identifierToken str, lexer''')
      c
        | isDigit c ->
            let (str, lexer''') = readInt lexer'
             in (Int str, lexer''')
      _ -> (Illegal, advance lexer')

-- Retorna o próximo caractere a ser lido
peek :: Lexer -> Maybe Char
peek Lexer{..}
  | readPosition < B.length input = Just (B.index input readPosition)
  | otherwise = Nothing

-- Avança a posição do lexer
advance :: Lexer -> Lexer
advance lexer@Lexer{..} =
  lexer
    { position = readPosition
    , readPosition = readPosition + 1
    , currentChar = fromMaybe '\0' $ peek lexer
    }

-- Pula os espaços em branco
skipSpace :: Lexer -> Lexer
skipSpace = skipWhile isSpace

-- Lê um identificador
readIdentifier :: Lexer -> (String, Lexer)
readIdentifier = readWhile isIdentifierChar

-- Lê um inteiro
readInt :: Lexer -> (String, Lexer)
readInt = readWhile isDigit

-- Lê enquanto o predicado for verdadeiro
readWhile :: (Char -> Bool) -> Lexer -> (String, Lexer)
readWhile predicate lexer@Lexer{..}
  | predicate currentChar = (currentChar : str, lexer')
  | otherwise = ("", lexer)
 where
  (str, lexer') = readWhile predicate $ advance lexer

-- Pula enquanto o predicado for verdadeiro
skipWhile :: (Char -> Bool) -> Lexer -> Lexer
skipWhile predicate lexer@Lexer{..}
  | predicate currentChar = skipWhile predicate $ advance lexer
  | otherwise = lexer
