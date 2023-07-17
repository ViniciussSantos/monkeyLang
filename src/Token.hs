module Token where


type AnalisadorLexico = String -> [Token]

data Token = Invalido -- Token Invalido
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
    | True            -- Boolean true
    | False           -- Boolean false
    | Equal           -- Operador de igualdade
    | NotEqual        -- Operador de desigualdade
    | Comma           -- Operador de virgula
    | Semicolon       -- Operador de ponto e vigula
    | LP              -- Parentese esquerdo
    | RP              -- Parentese direito
    | LC              -- Chave esquerdo
    | RC              -- Chave direito
    | Function        -- Palavra-chave funcion
    | Let             -- Palavra-chave let
    | If              -- Palavra-chave if
    | Else            -- Palavra-chave else
    | Return          -- Palavra-chave return
    deriving (Show, Eq, Ord)