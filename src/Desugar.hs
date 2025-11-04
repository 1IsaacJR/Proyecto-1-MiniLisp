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


-- Igualdad
desugar (EqS [])  = Bool True
desugar (EqS [_]) = Bool True
desugar (EqS (x:y:xs)) =
    foldl (\acc (a, b) -> And acc (Eq (desugar a) (desugar b)))
          (Eq (desugar x) (desugar y))
          (zip (y:xs) xs)

-- Desigualdad
desugar (NeqS [])  = Bool False
desugar (NeqS [_]) = Bool False
desugar (NeqS (x:y:xs)) =
    foldl (\acc (a, b) -> And acc (Neq (desugar a) (desugar b)))
          (Neq (desugar x) (desugar y))
          (zip (y:xs) xs)

-- Menor que
desugar (LtS [])  = Bool True
desugar (LtS [_]) = Bool True
desugar (LtS (x:y:xs)) =
    foldl (\acc (a, b) -> And acc (Lt (desugar a) (desugar b)))
          (Lt (desugar x) (desugar y))
          (zip (y:xs) xs)

-- Mayor que
desugar (GtS [])  = Bool True
desugar (GtS [_]) = Bool True
desugar (GtS (x:y:xs)) =
    foldl (\acc (a, b) -> And acc (Gt (desugar a) (desugar b)))
          (Gt (desugar x) (desugar y))
          (zip (y:xs) xs)

-- Menor o igual
desugar (LeqS [])  = Bool True
desugar (LeqS [_]) = Bool True
desugar (LeqS (x:y:xs)) =
    foldl (\acc (a, b) -> And acc (Leq (desugar a) (desugar b)))
          (Leq (desugar x) (desugar y))
          (zip (y:xs) xs)

-- Mayor o igual
desugar (GeqS [])  = Bool True
desugar (GeqS [_]) = Bool True
desugar (GeqS (x:y:xs)) =
    foldl (\acc (a, b) -> And acc (Geq (desugar a) (desugar b)))
          (Geq (desugar x) (desugar y))
          (zip (y:xs) xs)

-- Pares y listas
desugar (PairS a b) = Pair (desugar a) (desugar b)
desugar (FstS e) = Fst (desugar e)
desugar (SndS e) = Snd (desugar e)
desugar (ListS es) = List (map desugar es)
desugar (HeadS e) = Head (desugar e)
desugar (TailS e) = Tail (desugar e)
desugar NilS = Nil

-- Let normal
desugar (LetS bindings c) =
    let vars = map fst bindings
        vals = map (desugar . snd) bindings
    in App (Lambda vars (desugar c)) vals

-- LetStar secuencial
desugar (LetStarS [] c) = desugar c
desugar (LetStarS ((x,v):xs) c) =
    App (Lambda [x] (desugar (LetStarS xs c))) [desugar v]

desugar (LetRecS [(f, body)] expr) =
  App (Lambda [f] (desugar expr))
      [App robustCombinator [Lambda [f] (desugar body)]]


-- Condicionales
-- if es primitiva, se deja tal cual
desugar (IfS c t e) = If (desugar c) (desugar t) (desugar e)

-- if0 se convierte en if
desugar (If0S c t e) = 
    If (Eq (desugar c) (Num 0)) (desugar t) (desugar e)

desugar (CondS branches mElse) =
  case branches of
    [] -> maybe (error "Cond vacío sin else") desugar mElse
    ((c,b):xs) -> If (desugar c) (desugar b) (desugar (CondS xs mElse))


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
desugarV (And a b)     = AndV (desugarV a) (desugarV b)
desugarV (Or a b)      = OrV  (desugarV a) (desugarV b)

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
desugarV (If c t e) = IfV (desugarV c) (desugarV t) (desugarV e)

-- Let
desugarV (Let binds body) =
  -- (let ((x e)) body)  ==>  ((lambda (x) body) e)
  case binds of
    [] -> desugarV body
    ((x, e):xs) ->
      AppV
        (LambdaV [x] (desugarV (Let xs body)))  -- aplica recursivamente si hay más bindings
        [desugarV e]

desugarV x = error $ "Falta caso en desugarV: " ++ show x




robustCombinator :: ASA
robustCombinator =
  Lambda ["f"] $
    Let [("rec", Lambda ["self"] 
          (App (Var "f") 
               [Lambda ["x"] (App (App (Var "self") [Var "self"]) [Var "x"])]))]
        (App (Var "rec") [Var "rec"])

renameVars :: SASA -> String -> String -> SASA
renameVars (VarS oldName) old new = 
  if oldName == old then VarS new else VarS oldName

renameVars (AppS f args) old new = 
  AppS (renameVars f old new) (map (\x -> renameVars x old new) args)

renameVars (LambdaS params body) old new = 
  if old `elem` params 
    then LambdaS params body  -- No renombrar dentro de los parámetros
    else LambdaS params (renameVars body old new)

renameVars (LetS bindings body) old new =
  LetS (map (\(v,e) -> (v, renameVars e old new)) bindings) 
       (renameVars body old new)

renameVars (LetRecS bindings body) old new =
  LetRecS (map (\(v,e) -> 
                  if v == old 
                    then (new, renameVars e old new)  -- ¡RENOMBRAR también en los bindings!
                    else (v, renameVars e old new)) 
               bindings) 
          (renameVars body old new)

renameVars other _ _ = other
