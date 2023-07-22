module Lexer.Lexer where

import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isDigit, isSpace)
import Data.Maybe (fromMaybe)
import Token (Token (..), Tokenizer, identToken, isIdentChar)

-- Entrada de texto
type Input = ByteString

-- Estrutura de dados do Lexico
data Lexer = Lexer {
    input :: Input, 
    position :: Int,
    readPosition :: Int     
}

newLexer :: Input -> Lexer
newLexer input = Lexer {
    input = input,
    position = 0,
    readPosition = 0
}

nextToken :: Lexer -> (Token, Lexer)
nextToken lexer@Lexer{..} =
    case ch of
        '{' -> (LBrace, advance lexer)
        '}' -> (RBrace, advance lexer)
        '(' -> (LParen, advance lexer)
        ')' -> (RParen, advance lexer)
        ',' -> (Comma, advance lexer)
        ';' -> (Semicolon, advance lexer)
        '+' -> (Plus, advance lexer)
        '-' -> (Minus, advance lexer)
        '!' ->
            if peek == '='
                then (NotEqual, advance $ advance lexer)
                else (Bang, advance lexer)
        '>' -> (GreaterThan, advance lexer)
        '<' -> (LessThan, advance lexer)
        '*' -> (Asterisk, advance lexer)
        '/' -> (Slash, advance lexer)
        '=' ->
            if peek == '='
                then (Equal, advance $ advance lexer)
                else (Attrib, advance lexer)
        '\0' -> (EOF, lexer)
        c
            | isIdentChar c ->
                let (str, lexer') = readWhile isIdentChar lexer
                 in (identToken str, lexer')
        c
            | isDigit c ->
                let (str, lexer') = readWhile isDigit lexer
                 in (Int str, lexer')
        _ -> (Illegal, advance lexer)

peek :: Lexer -> Char
peek Lexer{..} = fromMaybe '\0' $ input BS.!? readPosition

tokenize :: Tokenizer
tokenize = go . advance . newLexer . BS.pack
    where
      go lexer = let (token, lexer') = nextToken lexer in
        case token of
            EOF -> [EOF]
            _   -> token : go lexer'

advance :: Lexer -> Lexer
advance lexer@Lexer{..} = lexer { 
    position = readPosition, 
    readPosition = readPosition + 1
}

readWhile :: (Char -> Bool) -> Lexer -> (String, Lexer)
readWhile p lexer@Lexer{..} =
    let (str, lexer') = go lexer
     in (ch : str, lexer')
  where
    go lexer@Lexer{..}
        | p ch = go (advance lexer)
        | otherwise = (ch : "", lexer)

skipWhitespace :: Lexer -> Lexer
skipWhitespace = readWhile isSpace