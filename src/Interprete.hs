module Interprete where

import ASAValues

-- Entorno
type Env = [(String, ASAValues)]

-- Paso pequeño (small step)
stp :: (ASAValues, Env) -> Maybe (ASAValues, Env)

-- ===== Valores base / lookup / closures =====

-- Variables: buscar en el entorno
stp (VarV x, env) = Just (lookupEnv x env, env)

-- Lambdas de superficie: cerrar con el entorno actual
stp (LambdaV ps body, env) = Just (ClosureV ps body env, env)

-- Valores atómicos/closures ya no reducen
stp (NumV _,  _)         = Nothing
stp (BoolV _, _)         = Nothing
stp (ClosureV _ _ _, _)  = Nothing
stp (NilV,   _)          = Nothing
stp (ListV xs, env)
  | all isValueV xs      = Nothing

-- ===== Aritmética =====
-- Suma
stp (AddV (NumV a) (NumV b), env) = Just (NumV (a + b), env)
stp (AddV a (NumV b), env)        = do (a', env') <- stp (a, env); return (AddV a' (NumV b), env')
stp (AddV (NumV a) b, env)        = do (b', env') <- stp (b, env); return (AddV (NumV a) b', env')
stp (AddV a b, env)               = do (a', env') <- stp (a, env); return (AddV a' b, env')

-- Resta
stp (SubV (NumV a) (NumV b), env) = Just (NumV (a - b), env)
stp (SubV a (NumV b), env)        = do (a', env') <- stp (a, env); return (SubV a' (NumV b), env')
stp (SubV (NumV a) b, env)        = do (b', env') <- stp (b, env); return (SubV (NumV a) b', env')
stp (SubV a b, env)               = do (a', env') <- stp (a, env); return (SubV a' b, env')

-- Multiplicación
stp (MulV (NumV a) (NumV b), env) = Just (NumV (a * b), env)
stp (MulV a (NumV b), env)        = do (a', env') <- stp (a, env); return (MulV a' (NumV b), env')
stp (MulV (NumV a) b, env)        = do (b', env') <- stp (b, env); return (MulV (NumV a) b', env')
stp (MulV a b, env)               = do (a', env') <- stp (a, env); return (MulV a' b, env')

-- División (entera)
stp (DivV (NumV a) (NumV b), env) = Just (NumV (a `div` b), env)
stp (DivV a (NumV b), env)        = do (a', env') <- stp (a, env); return (DivV a' (NumV b), env')
stp (DivV (NumV a) b, env)        = do (b', env') <- stp (b, env); return (DivV (NumV a) b', env')
stp (DivV a b, env)               = do (a', env') <- stp (a, env); return (DivV a' b, env')

-- add1 / sub1
stp (Add1V (NumV a), env) = Just (NumV (a + 1), env)
stp (Add1V a, env)        = do (a', env') <- stp (a, env); return (Add1V a', env')

stp (Sub1V (NumV a), env) = Just (NumV (a - 1), env)
stp (Sub1V a, env)        = do (a', env') <- stp (a, env); return (Sub1V a', env')

-- sqrt (entera, piso)
stp (SqrtV (NumV a), env) = Just (NumV (floor (sqrt (fromIntegral a))), env)
stp (SqrtV a, env)        = do (a', env') <- stp (a, env); return (SqrtV a', env')

-- expt
stp (ExptV (NumV a) (NumV b), env) = Just (NumV (a ^ b), env)
stp (ExptV a (NumV b), env)        = do (a', env') <- stp (a, env); return (ExptV a' (NumV b), env')
stp (ExptV (NumV a) b, env)        = do (b', env') <- stp (b, env); return (ExptV (NumV a) b', env')
stp (ExptV a b, env)               = do (a', env') <- stp (a, env); return (ExptV a' b, env')

-- ===== Comparadores =====
stp (EqV  (NumV a) (NumV b), env) = Just (BoolV (a == b), env)
stp (EqV  a (NumV b), env)        = do (a', env') <- stp (a, env); return (EqV  a' (NumV b), env')
stp (EqV  (NumV a) b, env)        = do (b', env') <- stp (b, env); return (EqV  (NumV a) b', env')
stp (EqV  a b, env)               = do (a', env') <- stp (a, env); return (EqV  a' b, env')

stp (LtV  (NumV a) (NumV b), env) = Just (BoolV (a <  b), env)
stp (LtV  a (NumV b), env)        = do (a', env') <- stp (a, env); return (LtV  a' (NumV b), env')
stp (LtV  (NumV a) b, env)        = do (b', env') <- stp (b, env); return (LtV  (NumV a) b', env')
stp (LtV  a b, env)               = do (a', env') <- stp (a, env); return (LtV  a' b, env')

stp (GtV  (NumV a) (NumV b), env) = Just (BoolV (a >  b), env)
stp (GtV  a (NumV b), env)        = do (a', env') <- stp (a, env); return (GtV  a' (NumV b), env')
stp (GtV  (NumV a) b, env)        = do (b', env') <- stp (b, env); return (GtV  (NumV a) b', env')
stp (GtV  a b, env)               = do (a', env') <- stp (a, env); return (GtV  a' b, env')

stp (LeqV (NumV a) (NumV b), env) = Just (BoolV (a <= b), env)
stp (LeqV a (NumV b), env)        = do (a', env') <- stp (a, env); return (LeqV a' (NumV b), env')
stp (LeqV (NumV a) b, env)        = do (b', env') <- stp (b, env); return (LeqV (NumV a) b', env')
stp (LeqV a b, env)               = do (a', env') <- stp (a, env); return (LeqV a' b, env')

stp (GeqV (NumV a) (NumV b), env) = Just (BoolV (a >= b), env)
stp (GeqV a (NumV b), env)        = do (a', env') <- stp (a, env); return (GeqV a' (NumV b), env')
stp (GeqV (NumV a) b, env)        = do (b', env') <- stp (b, env); return (GeqV (NumV a) b', env')
stp (GeqV a b, env)               = do (a', env') <- stp (a, env); return (GeqV a' b, env')

stp (NeqV (NumV a) (NumV b), env) = Just (BoolV (a /= b), env)
stp (NeqV a (NumV b), env)        = do (a', env') <- stp (a, env); return (NeqV a' (NumV b), env')
stp (NeqV (NumV a) b, env)        = do (b', env') <- stp (b, env); return (NeqV (NumV a) b', env')
stp (NeqV a b, env)               = do (a', env') <- stp (a, env); return (NeqV a' b, env')

-- ===== Condicionales =====
-- if booleano
stp (IfV (BoolV True)  t _, env) = Just (t, env)
stp (IfV (BoolV False) _ e, env) = Just (e, env)
stp (IfV c t e, env)             = do (c', env') <- stp (c, env); return (IfV c' t e, env')

-- if0 (numérico)
stp (If0V (NumV 0) t _, env) = Just (t, env)
stp (If0V (NumV _) _ e, env) = Just (e, env)
stp (If0V c t e, env)        = do (c', env') <- stp (c, env); return (If0V c' t e, env')

-- ===== Pares (con errores claros si no es par) =====
stp (PairV a b, env)
  | isValueV a && isValueV b = Nothing
  | isValueV a               = do (b', env') <- stp (b, env); return (PairV a b', env')
  | otherwise                = do (a', env') <- stp (a, env); return (PairV a' b, env')

-- fst
stp (FstV (PairV a _), env) = Just (a, env)
stp (FstV p, env)
  | isValueV p =
      case p of
        PairV a _ -> Just (a, env)                 -- redundante por seguridad
        _         -> error "fst: se esperaba un par"
  | otherwise  = do (p', env') <- stp (p, env); return (FstV p', env')

-- snd
stp (SndV (PairV _ b), env) = Just (b, env)
stp (SndV p, env)
  | isValueV p =
      case p of
        PairV _ b -> Just (b, env)
        _         -> error "snd: se esperaba un par"
  | otherwise  = do (p', env') <- stp (p, env); return (SndV p', env')

-- ===== Listas =====
stp (HeadV (ListV (x:_)), env) = Just (x, env)
stp (HeadV (ListV []),  _)     = error "head de lista vacía"
stp (HeadV xs, env)            = do (xs', env') <- stp (xs, env); return (HeadV xs', env')

stp (TailV (ListV (_:xs)), env) = Just (ListV xs, env)
stp (TailV (ListV []),  _)      = error "tail de lista vacía"
stp (TailV xs, env)             = do (xs', env') <- stp (xs, env); return (TailV xs', env')

-- ===== Aplicación de funciones (CBV: f primero, luego args) =====
stp (AppV f args, env)
  | not (isValueV f) = do
      (f', env') <- stp (f, env)
      return (AppV f' args, env')

stp (AppV f args, env)
  | any (not . isValueV) args = do
      (args', env') <- stpArgs args env
      return (AppV f args', env')

stp (AppV (ClosureV ps body envC) args, _)
  | length ps == length args
  , all isValueV args = Just (body, zip ps args ++ envC)

-- Si nada matchea, no hay paso
stp _ = Nothing

-- Reducir argumentos de izq->der
stpArgs :: [ASAValues] -> Env -> Maybe ([ASAValues], Env)
stpArgs [] env = Just ([], env)
stpArgs (x:xs) env
  | isValueV x = do
      (xs', env') <- stpArgs xs env
      return (x:xs', env')
  | otherwise = do
      (x', env') <- stp (x, env)
      return (x':xs, env')

-- Intérprete (evaluación hasta valor)
interp :: ASAValues -> Env -> ASAValues
interp e env
  | isValueV e = e
  | otherwise  =
      case stp (e, env) of
        Just (e', env') -> interp e' env'
        Nothing         -> e

-- Lookup
lookupEnv :: String -> Env -> ASAValues
lookupEnv i [] = error ("Variable " ++ i ++ " not found")
lookupEnv i ((j,v):env)
  | i == j    = v
  | otherwise = lookupEnv i env

-- ¿Es valor?
isValueV :: ASAValues -> Bool
isValueV (NumV _)         = True
isValueV (BoolV _)        = True
isValueV (ClosureV _ _ _) = True
isValueV (PairV a b)      = isValueV a && isValueV b
isValueV (ListV xs)       = all isValueV xs
isValueV NilV             = True
isValueV _                = False
