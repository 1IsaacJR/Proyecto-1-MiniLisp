module Desugar where

import SASA
import ASA

-- Función principal de desugaring
desugar :: SASA -> ASA
desugar (NumS n) = Num n
desugar (BoolS b) = Bool b
desugar (VarS v) = Var v

-- Operadores binarios/variádicos
desugar (AddS (x:xs)) = reduceLeft Add (desugar x : map desugar xs)
desugar (SubS (x:xs)) = reduceLeft Sub (desugar x : map desugar xs)
desugar (MulS (x:xs)) = reduceLeft Mul (desugar x : map desugar xs)
desugar (DivS (x:xs)) = reduceLeft Div (desugar x : map desugar xs)

-- Comparaciones variádicas (pares consecutivos)
desugar (EqS (x:xs))  = reduceLeft Eq (desugar x : map desugar xs)
desugar (LtS (x:xs))  = reduceLeft Lt (desugar x : map desugar xs)
desugar (GtS (x:xs))  = reduceLeft Gt (desugar x : map desugar xs)
desugar (LeqS (x:xs)) = reduceLeft Leq (desugar x : map desugar xs)
desugar (GeqS (x:xs)) = reduceLeft Geq (desugar x : map desugar xs)
desugar (NeqS (x:xs)) = reduceLeft Neq (desugar x : map desugar xs)

-- Unarios
desugar (Add1S e) = Add1 (desugar e)
desugar (Sub1S e) = Sub1 (desugar e)
desugar (SqrtS e) = Sqrt (desugar e)
desugar (ExptS a b) = Expt (desugar a) (desugar b)

-- Pares y listas
desugar (PairS a b) = Pair (desugar a) (desugar b)
desugar (FstS e) = Fst (desugar e)
desugar (SndS e) = Snd (desugar e)
desugar (ListS es) = List (map desugar es)
desugar (HeadS e) = Head (desugar e)
desugar (TailS e) = Tail (desugar e)
desugar NilS = Nil

-- Let normal
desugar (LetS [(x,v)] c) = App (Lambda [x] (desugar c)) [desugar v]

-- LetRec
desugar (LetRecS [(f,v)] c) = LetRec [(f, desugar v)] (desugar c)

-- LetStar
desugar (LetStarS [] c) = desugar c
desugar (LetStarS ((x,v):xs) c) =
    App (Lambda [x] (desugar (LetStarS xs c))) [desugar v]

-- Condicionales
desugar (IfS c t e) = If (desugar c) (desugar t) (desugar e)
desugar (If0S c t e) = If0 (desugar c) (desugar t) (desugar e)
desugar (CondS [] Nothing) = error "Cond vacío sin else"
desugar (CondS ((c,e):xs) mElse) =
    let elseBranch = case xs of
                        [] -> maybe (error "Cond sin else") desugar mElse
                        _  -> desugar (CondS xs mElse)
    in If (desugar c) (desugar e) elseBranch

-- Funciones y aplicaciones
desugar (LambdaS xs body) = Lambda xs (desugar body)
desugar (AppS f args) = App (desugar f) (map desugar args)

-- Función auxiliar para reducir operadores variádicos izquierda a derecha
reduceLeft :: (ASA -> ASA -> ASA) -> [ASA] -> ASA
reduceLeft f [x] = x
reduceLeft f (x:y:rest) = reduceLeft f (f x y : rest)
reduceLeft _ [] = error "reduceLeft: lista vacía"
