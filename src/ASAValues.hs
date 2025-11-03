module ASAValues where


data ASAValues
    = NumV Int
    | BoolV Bool
    | VarV String

    -- Operadores aritméticos y lógicos
    | AddV ASAValues ASAValues
    | SubV ASAValues ASAValues
    | MulV ASAValues ASAValues
    | DivV ASAValues ASAValues
    | EqV  ASAValues ASAValues
    | LtV  ASAValues ASAValues
    | GtV  ASAValues ASAValues
    | LeqV ASAValues ASAValues
    | GeqV ASAValues ASAValues
    | NeqV ASAValues ASAValues

    -- Operadores unarios
    | Add1V ASAValues
    | Sub1V ASAValues
    | SqrtV ASAValues
    | ExptV ASAValues ASAValues

    | AndV ASAValues ASAValues
    | OrV ASAValues ASAValues
    | NotV ASAValues


    | IfV ASAValues ASAValues ASAValues

    -- Pares y listas
    | PairV ASAValues ASAValues
    | FstV ASAValues
    | SndV ASAValues
    | ListV [ASAValues]
    | HeadV ASAValues
    | TailV ASAValues
    | NilV

    -- Funciones y closures
    | LambdaV [String] ASAValues
    | ClosureV [String] ASAValues [(String, ASAValues)]
    | AppV ASAValues [ASAValues]

    -- Expresiones diferidas
    | ExprV ASAValues [(String, ASAValues)]
    deriving (Show, Eq)