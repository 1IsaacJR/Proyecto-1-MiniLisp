module Interprete where

import ASAValues
import Desugar

type Env = [(String, ASAValues)]

-- Paso pequeño (small step)
stp :: (ASAValues, Env) -> Maybe (ASAValues, Env)

-- Valores atómicos que no reducen más
stp (NumV _, _) = Nothing
stp (BoolV _, _) = Nothing
stp (ClosureV _ _ _, _) = Nothing
stp (NilV, _) = Nothing
stp (ListV xs, _) | all isValueV xs = Nothing

-- Casos que reducen
stp (VarV n, env) = Just (lookupEnv n env, env)
stp (LambdaV p c, env) = Just (ClosureV p c env, env)

-- Operadores aritméticos

-- Suma
stp (AddV (NumV a) (NumV b), env) = Just (NumV (a + b), env)
stp (AddV a (NumV b), env) = do
    (a', env') <- stp (a, env)
    return (AddV a' (NumV b), env')
stp (AddV (NumV a) b, env) = do
    (b', env') <- stp (b, env)
    return (AddV (NumV a) b', env')
stp (AddV a b, env) = do
    (a', env') <- stp (a, env)
    return (AddV a' b, env')

-- Resta
stp (SubV (NumV a) (NumV b), env) = Just (NumV (a - b), env)
stp (SubV a (NumV b), env) = do
    (a', env') <- stp (a, env)
    return (SubV a' (NumV b), env')
stp (SubV (NumV a) b, env) = do
    (b', env') <- stp (b, env)
    return (SubV (NumV a) b', env')
stp (SubV a b, env) = do
    (a', env') <- stp (a, env)
    return (SubV a' b, env')

-- Multiplicación
stp (MulV (NumV a) (NumV b), env) = Just (NumV (a * b), env)
stp (MulV a (NumV b), env) = do
    (a', env') <- stp (a, env)
    return (MulV a' (NumV b), env')
stp (MulV (NumV a) b, env) = do
    (b', env') <- stp (b, env)
    return (MulV (NumV a) b', env')
stp (MulV a b, env) = do
    (a', env') <- stp (a, env)
    return (MulV a' b, env')

-- División
stp (DivV (NumV a) (NumV b), env) = Just (NumV (a `div` b), env)
stp (DivV a (NumV b), env) = do
    (a', env') <- stp (a, env)
    return (DivV a' (NumV b), env')
stp (DivV (NumV a) b, env) = do
    (b', env') <- stp (b, env)
    return (DivV (NumV a) b', env')
stp (DivV a b, env) = do
    (a', env') <- stp (a, env)
    return (DivV a' b, env')

-- Incremento 
stp (Add1V (NumV a), env) = Just (NumV (a + 1), env)
stp (Add1V a, env) = do
    (a', env') <- stp (a, env)
    return (Add1V a', env')

--Decremento
stp (Sub1V (NumV a), env) = Just (NumV (a - 1), env)
stp (Sub1V a, env) = do
    (a', env') <- stp (a, env)
    return (Sub1V a', env')

-- Raíz cuadrada
stp (SqrtV (NumV a), env) = Just (NumV (floor (sqrt (fromIntegral a))), env)
stp (SqrtV a, env) = do
    (a', env') <- stp (a, env)
    return (SqrtV a', env')

-- Potencia
stp (ExptV (NumV a) (NumV b), env) = Just (NumV (a ^ b), env)
stp (ExptV a (NumV b), env) = do
    (a', env') <- stp (a, env)
    return (ExptV a' (NumV b), env')
stp (ExptV (NumV a) b, env) = do
    (b', env') <- stp (b, env)
    return (ExptV (NumV a) b', env')
stp (ExptV a b, env) = do
    (a', env') <- stp (a, env)
    return (ExptV a' b, env')


-- igualdad
stp (EqV (NumV a) (NumV b), env) = Just (BoolV ( a == b), env) 
stp (EqV (NumV a) b, env) = do 
    (b', env') <- stp (b, env)
    return (EqV (NumV a) b', env')
stp (EqV a b, env) = do 
    (a', env') <- stp (a, env)
    return (EqV a' b, env')

-- Menor que
stp (LtV (NumV a) (NumV b), env) = Just (BoolV (a < b), env)
stp (LtV (NumV a) b, env) = do 
    (b', env') <- stp (b, env)
    return (LtV (NumV a) b', env')
stp (LtV a b, env) = do 
    (a', env') <- stp (a, env)
    return (LtV a' b, env')

-- Mayor que
stp (GtV (NumV a) (NumV b), env) = Just (BoolV (a > b), env) 
stp (GtV (NumV a) b, env) = do 
    (b', env') <- stp (b, env)
    return (GtV (NumV a) b', env')
stp (GtV a b, env) = do 
    (a', env') <- stp (a, env)
    return (GtV a' b, env')

--Menor igual que 
stp (LeqV (NumV a) (NumV b), env) = Just (BoolV (a <= b), env) 
stp (LeqV (NumV a) b, env) = do 
    (b', env') <- stp (b, env)
    return (LeqV (NumV a) b', env')
stp (LeqV a b, env) = do 
    (a', env') <- stp (a, env)
    return (LeqV a' b, env')

--Mayor igual que
stp (GeqV (NumV a) (NumV b), env) = Just (BoolV (a >= b), env) 
stp (GeqV (NumV a) b, env) = do 
    (b', env') <- stp (b, env)
    return (GeqV (NumV a) b', env')
stp (GeqV a b, env) = do 
    (a', env') <- stp (a, env)
    return (GeqV a' b, env')

-- DIferente de
stp (NeqV (NumV a) (NumV b), env) = Just (BoolV (a /= b), env) 
stp (NeqV (NumV a) b, env) = do 
    (b', env') <- stp (b, env)
    return (NeqV (NumV a) b', env')
stp (NeqV a b, env) = do 
    (a', env') <- stp (a, env)
    return (NeqV a' b, env')

 -- AND
stp (AndV (BoolV True) b, env)  = Just (b, env)
stp (AndV (BoolV False) _, env) = Just (BoolV False, env)
stp (AndV a b, env)
  | not (isValueV a) = do
      (a', env') <- stp (a, env)
      Just (AndV a' b, env')
  | otherwise = do
      (b', env') <- stp (b, env)
      Just (AndV a b', env')


-- OR
stp (OrV (BoolV True) _, env)   = Just (BoolV True, env)
stp (OrV (BoolV False) b, env)  = Just (b, env)
stp (OrV a b, env)
  | not (isValueV a) = do
      (a', env') <- stp (a, env)
      Just (OrV a' b, env')
  | otherwise = do
      (b', env') <- stp (b, env)
      Just (OrV a b', env')


-- NOT
stp (NotV (BoolV True), env)  = Just (BoolV False, env)
stp (NotV (BoolV False), env) = Just (BoolV True, env)
stp (NotV e, env)
  | not (isValueV e) = do
      (e', env') <- stp (e, env)
      Just (NotV e', env')
stp (NotV e, env) | isValueV e = Nothing


--Condicional 

-- condición ya es True
stp (IfV (BoolV True) t e, env) = Just (t, env)

-- condición ya es False
stp (IfV (BoolV False) t e, env) = Just (e, env)

--condición todavía no evaluada
stp (IfV c t e, env) = do
    (c', env') <- stp (c, env)  -- evaluamos un paso de la condición
    return (IfV c' t e, env')   -- reconstruimos el IfV con la condición parcialmente evaluada

-- Pares
stp (PairV a b, env)
    | isValueV a && isValueV b = Just (PairV a b, env)
    | isValueV a = do
        (b', env') <- stp (b, env)
        return (PairV a b', env')
    | otherwise = do
        (a', env') <- stp (a, env)
        return (PairV a' b, env')

stp (FstV (PairV a b), env) = Just (a, env)
stp (FstV a, env) = do
    (a', env') <- stp (a, env)
    return (FstV a', env')

stp (SndV (PairV a b), env) = Just (b, env)
stp (SndV a, env) = do
    (a', env') <- stp (a, env)
    return (SndV a', env')


-- Listas
stp (HeadV (ListV (x:_)), env) = Just (x, env)
stp (HeadV (ListV []), env) = Nothing
stp (HeadV xs, env) = do
    (xs', env') <- stp (xs, env)
    return (HeadV xs', env')

stp (TailV (ListV (_:xs)), env) = Just (ListV xs, env)
stp (TailV (ListV []), env) = Nothing
stp (TailV xs, env) = do
    (xs', env') <- stp (xs, env)
    return (TailV xs', env')

--App

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
  
stp (AppV (ClosureV [f] body env) args, _)
  | length [f] == length args
  , all isValueV args =
      let self = ClosureV [f] body env  -- La función se referencia a sí misma
          newEnv = zip [f] args ++ [(f, self)] ++ env
      in Just (body, newEnv)


stp (AppV (ClosureV ["f"] body envC) [arg], _) 
  | isValueV arg =
      let recEnv = ("f", ClosureV ["f"] body (("f", ClosureV ["f"] body envC) : envC)) : envC
      in Just (body, zip ["f"] [arg] ++ recEnv)

stp (AppV f args, env) | isValueV f && all isValueV args = Nothing


stpArgs :: [ASAValues] -> Env -> Maybe ([ASAValues], Env)
stpArgs [] env = Just ([], env)
stpArgs (x:xs) env
    | isValueV x = do
        (xs', env') <- stpArgs xs env
        return (x:xs', env')
    | otherwise = do
        (x', env') <- stp (x, env)
        return (x':xs, env')

-- Interprete (evaluación completa)
interp :: ASAValues -> Env -> ASAValues
interp e env
    | isValueV e = e
    | otherwise =
        case stp (e, env) of
            Just (e', env') -> interp e' env'
            Nothing -> e

-- Entorno
lookupEnv :: String -> Env -> ASAValues
lookupEnv i [] = error ("Variable " ++ i ++ " not found")
lookupEnv i ((j,v):env)
    | i == j = v
    | otherwise = lookupEnv i env

-- Valores
isValueV :: ASAValues -> Bool
isValueV (NumV _) = True
isValueV (BoolV _) = True
isValueV (ClosureV _ _ _) = True
isValueV (PairV a b) = isValueV a && isValueV b
isValueV (ListV xs) = all isValueV xs
isValueV _ = False
