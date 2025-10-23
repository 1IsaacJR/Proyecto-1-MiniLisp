module SASA where

-- Sintaxis superficial (SASA)
data SASA
    = NumS Int
    | BoolS Bool
    | VarS String

    -- Operadores variádicos (listas de argumentos)
    | AddS [SASA]
    | SubS [SASA]
    | MulS [SASA]
    | DivS [SASA]
    | EqS  [SASA]
    | LtS  [SASA]
    | GtS  [SASA]
    | LeqS [SASA]
    | GeqS [SASA]
    | NeqS [SASA]

    -- Unarios estilizados
    | Add1S SASA
    | Sub1S SASA
    | SqrtS SASA
    | ExptS SASA SASA

    -- Pares y listas
    | PairS SASA SASA
    | FstS SASA
    | SndS SASA
    | ListS [SASA]
    | HeadS SASA
    | TailS SASA
    | NilS

    -- Asignaciones (formas superficiales)
    | LetS [(String, SASA)] SASA
    | LetRecS [(String, SASA)] SASA
    | LetStarS [(String, SASA)] SASA

    -- Condicionales
    | IfS SASA SASA SASA
    | If0S SASA SASA SASA
    | CondS [(SASA, SASA)] (Maybe SASA)

    -- Funciones / Aplicaciones
    | LambdaS [String] SASA
    | AppS SASA [SASA]
    deriving (Show, Eq)

