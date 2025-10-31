module ASA where

-- Árbol de Sintaxis Abstracta (ASA)
data ASA
    = Num Int
    | Bool Bool
    | Var String

    -- Operadores aritméticos y lógicos
    | Add ASA ASA
    | Sub ASA ASA
    | Mul ASA ASA
    | Div ASA ASA
    | Eq  ASA ASA
    | Lt  ASA ASA
    | Gt  ASA ASA
    | Leq ASA ASA
    | Geq ASA ASA
    | Neq ASA ASA

    -- Operadores unarios
    | Add1 ASA
    | Sub1 ASA
    | Sqrt ASA
    | Expt ASA ASA

    | And ASA ASA
    | Or ASA ASA
    | Not ASA

    -- Pares y listas
    | Pair ASA ASA
    | Fst ASA
    | Snd ASA
    | List [ASA]      -- [1,2,3]
    | Head ASA
    | Tail ASA
    | Nil             -- lista vacía

    -- Asignaciones
    | Let   [(String, ASA)] ASA     -- let ((x e1) (y e2)) e3
    | LetRec [(String, ASA)] ASA    -- letrec ((f def)) cuerpo
    | LetStar [(String, ASA)] ASA   -- let* ((x e1) (y e2)) e3

    -- Condicionales
    | If ASA ASA ASA
    | If0 ASA ASA ASA
    | Cond [(ASA, ASA)] (Maybe ASA) -- cond [(c1,e1) ... (cn,en)] else?

    -- Funciones
    | Lambda [String] ASA   -- lambdas variádicas
    | App ASA [ASA]         -- aplicación a múltiples args
    deriving (Show, Eq)



