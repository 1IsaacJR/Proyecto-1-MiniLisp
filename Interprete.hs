--Se ha compilado, no funciona
module Interprete where
import ASA

{-
--Tipos de Closure, para aplicaciones, funciones y construcciones
data DifClosure = P Closure -- cabeza de la funcion
                | C Closure -- Cuerpo de la funcion
                | A Closure -- Armgumento de la funcion
-}

stp :: (ASA, Env) -> Maybe (ASA, Env)
stp (Num m ,env) = Just (Num m ,env)
stp (Bool a,env) = Just (Bool a ,env)
stp (Var n ,env) = Just (lookUp n env,env)
-- Agregar funciones como valores dentro del lenguaje
stp (Lambda p c, env) = Just (Closure p c env, env)
--stp(List x:xs, env) = (list x:xs, env)
                                                                                            --Operadores aritmeticos
--Reglas de Add
stp (Add (Num a) (Num b),env) = 
    Just (Num (a + b),env)
stp (Add a (Num b), env) = do 
    (a', env') <- stp (a, env)
    return (Add a' (Num b), env')
stp (Add a b, env) = do 
    (a', env') <- stp (a, env)
    return (Add a' b, env')
--Reglas de Sub
stp (Sub (Num a) (Num b),env) = Just (Num (a - b),env)
stp (Sub a (Num b), env) = do 
    (a', env') <- stp (a, env)
    return (Sub a' (Num b), env')
stp (Sub a b, env) = do 
    (a', env') <- stp (a, env)
    return (Sub a' b, env')
--Reglas Mul
stp (Mul (Num a) (Num b),env) = Just (Num (a * b),env)
stp (Mul a (Num b),env) = do 
    (a', env') <- stp (a, env)
    return (Mul a' (Num b), env')
stp (Mul a b, env) = do 
    (a', env') <- stp (a, env)
    return (Mul a' b, env')
--Reglas Div
stp (Div (Num a) (Num b),env) = Just (Num (a / b),env)
stp (Div a (Num b),env) = do 
    (a', env') <- stp (a, env)
    return (Div a' (Num b), env')
stp (Div a b, env) = do 
    (a', env') <- stp (a, env)
    return (Div a' b, env')
--Reglas Raiz
--Definir double para que arranque o ver como
--Reglas add1
stp (Add1 (Num a),env) = Just (Num (a+1),env)
stp (Add1 (a),env) = do 
    (a',env') <- stp (a,env)
    return (Add1 a',env')
--Reglas sub1
stp (Sub1 (Num a),env) = Just (Num (a-1),env)
stp (Sub1 a,env) = do 
    (a',env') <- stp (a,env)
    return (Sub1 a',env')
--Reglas de potencia
stp (Expt (Num a) (Num b), env)
    | b < 0     = Nothing  -- Si es negativo no lo cuenta
    | otherwise = Just (Num (a ^ b), env) --Mismas reglas que los operadores
stp (Expt a (Num b), env) = do 
    (a', env') <- stp (a, env)
    return (Expt a' (Num b), env')
stp (Expt a b, env) = do 
    (b', env') <- stp (b, env)
    return (Expt a b', env')
                                                                                            --Operadores Logicos
--Ver si se reducen a un valor, no solo a numeros
-- igualdad
stp (Eq (Num a) (Num b), env) = Just (Bool ( a == b), env) 
stp (Eq (Num a) b, env) = do 
    (b', env') <- stp (b, env)
    return (Eq (Num a) b', env')
stp (Eq a b, env) = do 
    (a', env') <- stp (a, env)
    return (Eq a' b, env')
-- Menor que
stp (Lt (Num a) (Num b), env) = Just (Bool (a < b), env)
stp (Lt (Num a) b, env) = do 
    (b', env') <- stp (b, env)
    return (Lt (Num a) b', env')
stp (Lt a b, env) = do 
    (a', env') <- stp (a, env)
    return (Lt a' b, env')
-- Mayor que
stp (Gt (Num a) (Num b), env) = Just (Bool (a > b), env) 
stp (Gt (Num a) b, env) = do 
    (b', env') <- stp (b, env)
    return (Gt (Num a) b', env')
stp (Gt a b, env) = do 
    (a', env') <- stp (a, env)
    return (Gt a' b, env')
--Menor igual que 
stp (Leq (Num a) (Num b), env) = Just (Bool (a <= b), env) 
stp (Leq (Num a) b, env) = do 
    (b', env') <- stp (b, env)
    return (Leq (Num a) b', env')
stp (Leq a b, env) = do 
    (a', env') <- stp (a, env)
    return (Leq a' b, env')
--Mayor igual que
stp (Geq (Num a) (Num b), env) = Just (Bool (a >= b), env) 
stp (Geq (Num a) b, env) = do 
    (b', env') <- stp (b, env)
    return (Geq (Num a) b', env')
stp (Geq a b, env) = do 
    (a', env') <- stp (a, env)
    return (Geq a' b, env')
-- DIferente de
stp (Neq (Num a) (Num b), env) = Just (Bool (a /= b), env) 
stp (Neq (Num a) b, env) = do 
    (b', env') <- stp (b, env)
    return (Neq (Num a) b', env')
stp (Neq a b, env) = do 
    (a', env') <- stp (a, env)
    return (Neq a' b, env')
                                                                                       --Funciones de listas

--Head (list)
stp (Head (List (a:_)), env) =  Just (a, env)
stp (Head e, env)
  | not (isValue e) = do
    (e', env') <- stp (e, env)
    return (Head e', env')
stp (Head (List []), env) = Just (Error "Empty list", env)

--Tail (list)
stp (Tail (List (c:xs)), env) = Just (List xs, env)
stp (Tail (List []), env) = Nothing
stp (Tail a, env) = do 
    (a',env') <- stp (a,env)
    return (Tail (a'),env') -- ??
                                                                    --Pares Ordenados y funciones de pares ordenados
--Pares ordenados / agregar lo de si ya es valor en el lenguje
stp ((Pair a b), env) -- Ver que ya no se reduce tanto
    | isValue a && isValue b = Just (Pair (a,b),env)
stp (Pair a b, env)
    | isValue a == True = do
    (b', env') <- stp (b, env)
    return ((Pair a b'), env')
stp ((Pair a b), env) = do 
    (a', env') <- stp (a,env)
    return ((Pair a' b), env')

--Primer elemento
stp (Fst a b, env)
    | isValue a == True = Just (a,env)
stp (Fst a b,env) = do 
    (a', env') <- stp (a,env)
    return (Fst a' b,env')

--Segundo elemento
stp (Snd a b , env)
    | isValue b == True = Just (b,env)
stp (Snd a b,env) = do 
    (b', env') <- stp (b,env)
    return (Snd a b',env')
                                                                        -- Condicionales
-- If (cond, cero y varidicos es azucar)
stp (If (Bool True) b c, env) =  Just (b, env)
stp (If (Bool False) b c, env) = Just (c, env)
stp (If a b c, env) = do 
    (a',env') <- stp (a,env)
    return (If a' b c ,env')

-- Aplicaciones de funcion
stp (App (Closure p c e) a,env)
    | all isValue a = Just (c, zip p a ++ e)
stp (App f a, env)
    | not (all isValue a) = do
        (a', env') <- stpAux (a env)
        return (App f a', env')
stp (App f a,env) = do
    (f',env') <- stp (f,env)
    return (App f' a, env')


stpAux :: [ASA] -> [(String, ASA)] -> Maybe ([ASA], [(String, ASA)])
stpAux [] env = Just ([], env)
stpAux (a:as) env
  | isValue a = do
      (as', env') <- stpAux as env
      return (a:as', env')
  | otherwise = do
      (a', env') <- stp (a, env)
      return (a':as, env')

interprete:: ASA -> Env -> ASA
interprete e env
    | isValue e == True = e
    | otherwise =  
        let (e',env') = stp (e, env)
        in interprete e' env'

lookUp :: String -> Env -> ASA
lookUp i [] = error ("Variable " ++ i ++ " not found")
lookUp i ((j, v) : env)
    | i == j = v
    | otherwise = lookUp i env

isValue :: ASA -> Bool
isValue (Num _) = True
isValue (Bool _) = True
isValue (Closure _ _ _) = True
isValue (Pair a b) = isValue a && isValue b
isValue _ = False


