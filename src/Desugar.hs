module Desugar (desugar, desugarV) where

import ASA
import ASAValues

-- ------------------------------------------------------------------
-- desugar :: ASA (superficie) -> ASA (core)
--  - Convierte let variádico paralelo a App(Lambda ...)
--  - Convierte let* en anidamiento de lets (vía App/Lambda)
--  - Convierte cond en cascada de if (requiere else)
--  - Recorre recursivamente el árbol
--  - El resto de formas se mantienen (solo se desazucaran subexpresiones)
-- ------------------------------------------------------------------

desugar :: ASA -> ASA
-- Básicos
desugar (Num n)   = Num n
desugar (Bool b)  = Bool b
desugar (Var x)   = Var x

-- Aritmética/Comparadores/Unarios (binarios ya en ASA)
desugar (Add a b)  = Add  (desugar a) (desugar b)
desugar (Sub a b)  = Sub  (desugar a) (desugar b)
desugar (Mul a b)  = Mul  (desugar a) (desugar b)
desugar (Div a b)  = Div  (desugar a) (desugar b)
desugar (Eq a b)   = Eq   (desugar a) (desugar b)
desugar (Lt a b)   = Lt   (desugar a) (desugar b)
desugar (Gt a b)   = Gt   (desugar a) (desugar b)
desugar (Leq a b)  = Leq  (desugar a) (desugar b)
desugar (Geq a b)  = Geq  (desugar a) (desugar b)
desugar (Neq a b)  = Neq  (desugar a) (desugar b)
desugar (Add1 e)   = Add1 (desugar e)
desugar (Sub1 e)   = Sub1 (desugar e)
desugar (Sqrt e)   = Sqrt (desugar e)
desugar (Expt a b) = Expt (desugar a) (desugar b)

-- Pares y listas
desugar (Pair a b)  = Pair (desugar a) (desugar b)
desugar (Fst e)     = Fst  (desugar e)
desugar (Snd e)     = Snd  (desugar e)
desugar (List es)   = List (map desugar es)
desugar (Head e)    = Head (desugar e)
desugar (Tail e)    = Tail (desugar e)
desugar Nil         = Nil

-- Let paralelo variádico: let ((x e1) (y e2) ...) body
desugar (Let binds body) =
  App (Lambda (map fst binds) (desugar body))
      (map (desugar . snd) binds)

-- Let* secuencial: let* ((x e1) (y e2) ...) body
desugar (LetStar [] body) = desugar body
desugar (LetStar ((x,e):xs) body) =
  App (Lambda [x] (desugar (LetStar xs body)))
      [desugar e]

-- Let recursivo (mantén su forma; si luego quieres, puedes atar el nudo en el intérprete)
desugar (LetRec defs body) =
  LetRec [ (f, desugar e) | (f,e) <- defs ] (desugar body)

-- Condicionales
desugar (If c t e)   = If  (desugar c) (desugar t) (desugar e)
desugar (If0 c t e)  = If0 (desugar c) (desugar t) (desugar e)

-- cond [(c1,e1) ... (cn,en)] else eElse  ==> if c1 e1 (if c2 e2 (... eElse))
desugar (Cond [] Nothing) = error "cond: falta rama else"
desugar (Cond [] (Just eElse)) = desugar eElse
desugar (Cond ((c,e):rest) mElse) =
  let elseBranch = case rest of
        [] -> maybe (error "cond: falta rama else") desugar mElse
        _  -> desugar (Cond rest mElse)
  in If (desugar c) (desugar e) elseBranch

-- Funciones y aplicación
desugar (Lambda xs body) = Lambda xs (desugar body)
desugar (App f args)     = App (desugar f) (map desugar args)

-- ------------------------------------------------------------------
-- desugarV :: ASA (core) -> ASAValues (forma evaluable por el intérprete)
--  - Mapea If -> IfV (booleans) e If0 -> If0V (numérico)
--  - NO genera ExprV raros (cond ya fue desazucarado arriba)
-- ------------------------------------------------------------------

desugarV :: ASA -> ASAValues
-- Básicos
desugarV (Num n)  = NumV n
desugarV (Bool b) = BoolV b
desugarV (Var x)  = VarV x

-- Aritmética/Comparadores
desugarV (Add a b) = AddV (desugarV a) (desugarV b)
desugarV (Sub a b) = SubV (desugarV a) (desugarV b)
desugarV (Mul a b) = MulV (desugarV a) (desugarV b)
desugarV (Div a b) = DivV (desugarV a) (desugarV b)
desugarV (Eq a b)  = EqV  (desugarV a) (desugarV b)
desugarV (Lt a b)  = LtV  (desugarV a) (desugarV b)
desugarV (Gt a b)  = GtV  (desugarV a) (desugarV b)
desugarV (Leq a b) = LeqV (desugarV a) (desugarV b)
desugarV (Geq a b) = GeqV (desugarV a) (desugarV b)
desugarV (Neq a b) = NeqV (desugarV a) (desugarV b)

-- Unarios
desugarV (Add1 e)   = Add1V (desugarV e)
desugarV (Sub1 e)   = Sub1V (desugarV e)
desugarV (Sqrt e)   = SqrtV (desugarV e)
desugarV (Expt a b) = ExptV (desugarV a) (desugarV b)

-- Pares y listas
desugarV (Pair a b) = PairV (desugarV a) (desugarV b)
desugarV (Fst e)    = FstV  (desugarV e)
desugarV (Snd e)    = SndV  (desugarV e)
desugarV (List es)  = ListV (map desugarV es)
desugarV (Head e)   = HeadV (desugarV e)
desugarV (Tail e)   = TailV (desugarV e)
desugarV Nil        = NilV

-- Condicionales
desugarV (If c t e)   = IfV  (desugarV c) (desugarV t) (desugarV e)
desugarV (If0 c t e)  = If0V (desugarV c) (desugarV t) (desugarV e)
desugarV (Cond _ _)   = error "desugarV: 'Cond' debe haberse desazucarado en 'desugar'"

-- Funciones y aplicación
desugarV (Lambda xs body) = LambdaV xs (desugarV body)
desugarV (App f args)     = AppV (desugarV f) (map desugarV args)

-- Let/Let*/LetRec (después de 'desugar' ya deberían venir como App/Lambda salvo LetRec)
desugarV (Let _ _)      = error "desugarV: Let debe desazucararse en 'desugar'"
desugarV (LetStar _ _)  = error "desugarV: Let* debe desazucararse en 'desugar'"
desugarV (LetRec defs body) =
  -- Representación directa en valores; el atado del nudo lo maneja tu intérprete o un paso adicional.
  -- Si prefieres, puedes mantener LetRec en ASA y no bajarlo a ASAValues hasta aplicarlo.
  -- Aquí lo dejamos como clausuras diferidas:
  AppV (LambdaV (map fst defs) (desugarV body))
       (map (desugarV . snd) defs)
