module Desugar where
import ASA

-- Función principal
desugar :: ASA -> ASA
desugar (LetStar ((x,e):xs) body) =
    Let [(x,e)] (desugar (LetStar xs body))  -- desazucariza en let anidados
desugar (LetStar [] body) = desugar body

desugar (Cond ((c,e):cs) elsePart) =
    If c (desugar e) (desugar (Cond cs elsePart))
desugar (Cond [] (Just e)) = desugar e
desugar (Cond [] Nothing)  = ErrorV "Cond sin else" `seq` Nil

-- Listas: [1,2,3] → Pair 1 (Pair 2 (Pair 3 Nil))
desugar (List (x:xs)) = Pair (desugar x) (desugar (List xs))
desugar (List [])     = Nil

-- Lambdas variádicas → currificación
desugar (Lambda (x:xs) body) =
    Lambda [x] (desugar (Lambda xs body))
desugar (Lambda [] body) = desugar body

-- Operadores variádicos (+ 1 2 3) → (+ 1 (+ 2 3))
desugar (Add a b) = Add (desugar a) (desugar b)
desugar (Sub a b) = Sub (desugar a) (desugar b)
desugar (Mul a b) = Mul (desugar a) (desugar b)
desugar (Div a b) = Div (desugar a) (desugar b)
desugar (Eq a b)  = Eq  (desugar a) (desugar b)

-- Recursión sobre el resto de casos
desugar (If c t e)      = If (desugar c) (desugar t) (desugar e)
desugar (If0 c t e)     = If0 (desugar c) (desugar t) (desugar e)
desugar (Pair x y)      = Pair (desugar x) (desugar y)
desugar (Fst p)         = Fst (desugar p)
desugar (Snd p)         = Snd (desugar p)
desugar (Head p)        = Head (desugar p)
desugar (Tail p)        = Tail (desugar p)
desugar (App f args)    = App (desugar f) (map desugar args)

-- Casos base
desugar (Num n)         = Num n
desugar (Bool b)        = Bool b
desugar (Var v)         = Var v
desugar Nil             = Nil
desugar x               = x
