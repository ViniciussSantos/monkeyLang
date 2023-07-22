module Token where

import Data.Char (isLetter)

type Tokenizer = String -> [Token]

data Token = 
      Illegal         -- Token Invalido
    | EOF             -- Fim do Arquivo
    | Ident String    -- Indetificadores
    | Int String      -- Numeros inteiros
    | Attrib          -- Operador de atribuicao
    | Plus            -- Operador de adicao
    | Minus           -- Operador de subtracao
    | Bang            -- Operador de negacao
    | Asterisk        -- Operador de multiplicacao
    | Slash           -- Operador de divisao
    | LessThan        -- Operador de menor
    | GreaterThan     -- Operador de maior
    | TokTrue         -- Boolean true
    | TokFalse        -- Boolean false
    | Equal           -- Operador de igualdade
    | NotEqual        -- Operador de desigualdade
    | Comma           -- Operador de virgula
    | Semicolon       -- Operador de ponto e vigula
    | LParen          -- Parentese esquerdo
    | RParen          -- Parentese direito
    | LBrace          -- Chave esquerdo
    | RBrace          -- Chave direito
    | Function        -- Palavra-chave funcion
    | Let             -- Palavra-chave let
    | If              -- Palavra-chave if
    | Else            -- Palavra-chave else
    | Return          -- Palavra-chave return
    deriving (Show, Eq, Ord)


-- Verifica se é um caractere
identChar :: Char -> Bool
identChar char = isLetter char || char == '_'

-- Verifica token
identToken :: String -> Token
identToken "true"  = TokTrue
identToken "false" = TokFalse
identToken "fn"    = Function
identToken "let"   = Let
identToken "if"    = If
identToken "else"  = Else
identToken "return" = Return
identToken x = Ident x