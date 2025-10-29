module Interprete where

import ASAValues
import Desugar

type Env = [(String, ASAValues)]

-- Paso pequeño (small step)
stp :: (ASAValues, Env) -> Maybe (ASAValues, Env)

-- Valores básicos
--stp (NumV m ,env) = Just (NumV m ,env)
--stp (BoolV a,env) = Just (BoolV a ,env)
--stp (VarV n ,env) = Just (lookupEnv n env,env)
--stp (LambdaV p c, env) = Just (ClosureV p c env, env)

stp (NumV m, env) = Nothing
stp (BoolV a, env) = Nothing
stp (ClosureV _ _ _, env) = Nothing

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

-- Incremento y decremento
stp (Add1V (NumV a), env) = Just (NumV (a + 1), env)
stp (Add1V a, env) = do
    (a', env') <- stp (a, env)
    return (Add1V a', env')

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





-- Operadores lógicos
stp (EqV (NumV a) (NumV b), env) = Just (BoolV (a == b), env)
stp (EqV a b, env) = do
    (a', env') <- stp (a, env)
    return (EqV a' b, env')

stp (LtV (NumV a) (NumV b), env) = Just (BoolV (a < b), env)
stp (LtV a b, env) = do
    (a', env') <- stp (a, env)
    return (LtV a' b, env')

stp (GtV (NumV a) (NumV b), env) = Just (BoolV (a > b), env)
stp (GtV a b, env) = do
    (a', env') <- stp (a, env)
    return (GtV a' b, env')

-- Condicional
stp (If0V (NumV 0) t e, env) = Just (t, env)
stp (If0V (NumV n) t e, env) = Just (e, env)
stp (If0V c t e, env) = do
    (c', env') <- stp (c, env)
    return (If0V c' t e, env')






    

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

-- Aplicación de funciones
stp (AppV (ClosureV p body envC) args, env)
    | all isValueV args = Just (body, zip p args ++ envC)
stp (AppV f args, env)
    | not (all isValueV args) = do
        (args', env') <- stpArgs args env
        return (AppV f args', env')
stp (AppV f args, env) = do
    (f', env') <- stp (f, env)
    return (AppV f' args, env')

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
