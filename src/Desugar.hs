module Desugar where

import SASA
import ASA
import ASAValues

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


-- Unarios
desugar (Add1S e) = Add1 (desugar e)
desugar (Sub1S e) = Sub1 (desugar e)
desugar (SqrtS e) = Sqrt (desugar e)
desugar (ExptS a b) = Expt (desugar a) (desugar b)


-- Comparaciones variádicas interpretadas como cadena (a < b < c ...)
desugar (EqS (x:y:xs)) =
    foldl (\acc (a, b) -> And acc (Eq (desugar a) (desugar b)))
          (Eq (desugar x) (desugar y))
          (zip (y:xs) xs)
desugar (EqS [x]) = desugar x
desugar (EqS [])  = error "comparación vacía (=)"

desugar (LtS (x:y:xs)) =
    foldl (\acc (a, b) -> And acc (Lt (desugar a) (desugar b)))
          (Lt (desugar x) (desugar y))
          (zip (y:xs) xs)
desugar (LtS [x]) = desugar x
desugar (LtS [])  = error "comparación vacía (<)"

desugar (GtS (x:y:xs)) =
    foldl (\acc (a, b) -> And acc (Gt (desugar a) (desugar b)))
          (Gt (desugar x) (desugar y))
          (zip (y:xs) xs)
desugar (GtS [x]) = desugar x
desugar (GtS [])  = error "comparación vacía (>)"

desugar (LeqS (x:y:xs)) =
    foldl (\acc (a, b) -> And acc (Leq (desugar a) (desugar b)))
          (Leq (desugar x) (desugar y))
          (zip (y:xs) xs)
desugar (LeqS [x]) = desugar x
desugar (LeqS [])  = error "comparación vacía (<=)"

desugar (GeqS (x:y:xs)) =
    foldl (\acc (a, b) -> And acc (Geq (desugar a) (desugar b)))
          (Geq (desugar x) (desugar y))
          (zip (y:xs) xs)
desugar (GeqS [x]) = desugar x
desugar (GeqS [])  = error "comparación vacía (>=)"

desugar (NeqS (x:y:xs)) =
    foldl (\acc (a, b) -> And acc (Neq (desugar a) (desugar b)))
          (Neq (desugar x) (desugar y))
          (zip (y:xs) xs)
desugar (NeqS [x]) = desugar x
desugar (NeqS [])  = error "comparación vacía (!=)"

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
desugar (LetRecS name body expr) =
    -- letrec x = e1 in e2  ≡  let x = Z (\x -> e1) in e2
    LetS name (App zCombinator (Lambda name (desugar body))) (desugar expr)




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

-- Convierte ASA a ASAValues
desugarV :: ASA -> ASAValues
desugarV (Num n)       = NumV n
desugarV (Bool b)      = BoolV b
desugarV (Var x)       = VarV x

-- Aritmética y lógica
desugarV (Add e1 e2)   = AddV (desugarV e1) (desugarV e2)
desugarV (Sub e1 e2)   = SubV (desugarV e1) (desugarV e2)
desugarV (Mul e1 e2)   = MulV (desugarV e1) (desugarV e2)
desugarV (Div e1 e2)   = DivV (desugarV e1) (desugarV e2)
desugarV (Eq e1 e2)    = EqV  (desugarV e1) (desugarV e2)
desugarV (Lt e1 e2)    = LtV  (desugarV e1) (desugarV e2)
desugarV (Gt e1 e2)    = GtV  (desugarV e1) (desugarV e2)
desugarV (Leq e1 e2)   = LeqV (desugarV e1) (desugarV e2)
desugarV (Geq e1 e2)   = GeqV (desugarV e1) (desugarV e2)
desugarV (Neq e1 e2)   = NeqV (desugarV e1) (desugarV e2)
desugarV (And a b)      =AndV (desugarV a) (desugarV b) 
desugarV (Or a b)     = OrV  (desugarV a) (desugarV b )

-- Unarios
desugarV (Add1 e)      = Add1V (desugarV e)
desugarV (Sub1 e)      = Sub1V (desugarV e)
desugarV (Sqrt e)      = SqrtV (desugarV e)
desugarV (Expt e1 e2)  = ExptV (desugarV e1) (desugarV e2)

-- Pares y listas
desugarV (Pair e1 e2)  = PairV (desugarV e1) (desugarV e2)
desugarV (Fst e)       = FstV (desugarV e)
desugarV (Snd e)       = SndV (desugarV e)
desugarV (List es)     = ListV (map desugarV es)
desugarV (Head e)      = HeadV (desugarV e)
desugarV (Tail e)      = TailV (desugarV e)
desugarV Nil           = NilV

-- Funciones y aplicación
desugarV (Lambda args body) = LambdaV args (desugarV body)
desugarV (App f args)       = AppV (desugarV f) (map desugarV args)

-- Condicionales
desugarV (If c t e)         = If0V (desugarV c) (desugarV t) (desugarV e) -- podrías crear IfV si quieres
desugarV (If0 c t e)        = If0V (desugarV c) (desugarV t) (desugarV e)
desugarV (Cond branches mElse) =
    let desBranches = map (\(c,b) -> (desugarV c, desugarV b)) branches
        elseV = fmap desugarV mElse
    in ExprV (ListV (map snd desBranches ++ maybe [] (:[]) elseV)) [] -- ejemplo, se puede ajustar

    zCombinator :: ASA
zCombinator =
    Lambda "f" $
      App
        (Lambda "x" (App (Var "f") (App (Var "x") (Var "x"))))
        (Lambda "x" (App (Var "f") (App (Var "x") (Var "x"))))