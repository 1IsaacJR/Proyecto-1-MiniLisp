--No se ha compilado, puede que no funcione
import Desugar

type Env = [(String, ASAValues)]

--Tipos de Closure, para aplicaciones, funciones y construcciones
data DifClosure = F Closure
                | N Closure
                | P Closure

stp :: (Expr, Env) -> Maybe (Expr, Env)
                                                                                                    -- Valores atomicos
    stp(Num m ,env) = (Num m ,env)
    stp(Bool a,env) = (Bool a ,env)
    stp(Var n ,env) = (lookup n env,env)
    stp(String s,env) = (String s,env)
    -- Agregar funciones como valores dentro del lenguaje
    stp (Fun p c) env = (Closure p c env, env)
    --stp(List x:xs, env) = (list x:xs, env)
                                                                                                --Operadores aritmeticos
    --Reglas de Add
    stp (Add (Num a) (Num b),env) = Just (Num (a + b),env)
    stp (Add a (Num b)) = do (a', env') <- stp (a, env)
        return (Add a' (Num b), env')
    stp (Add a b, env) = do (a', env') <- stp (a, env)
        return (Add a' b, env')

    --Reglas de Sub
    stp (Sub (Num a) (Num b),env) = Just (Num (a - b),env)
    stp (Sub a (Num b)) = do (a', env') <- stp (a, env)
        return (Sub a' (Num b), env')
    stp (Sub a b, env) = do (a', env') <- stp (a, env)
        return (Sub a' b, env')

    --Reglas Mult
    stp (Mult (Num a) (Num b),env) = Just (Mult (a * b),env)
    stp (Mult a (Num b)) = do (a', env') <- stp (a, env)
        return (Mult a' (Num b), env')
    stp (Mult a b, env) = do (a', env') <- stp (a, env)
        return (Mult a' b, env')

    --Reglas Div
    stp (Div (Num a) (Num b),env) = Just (Div (a / b),env)
    stp (Div a (Num b)) = do (a', env') <- stp (a, env)
        return (Div a' (Num b), env')
    stp (Div a b, env) = do (a', env') <- stp (a, env)
        return (Div a' b, env')

    --Reglas Raiz
    --Definir booleano para que arranque o ver como

    --Reglas de potencia
    stp (Pow (Num a) (Num b), env)
        | b < 0     = Nothing  -- Si es negativo no lo cuenta
        | otherwise = Just (Num (a ^ b), env) --Mismas reglas que los operadores
    stp (Pow a (Num b), env) = do (a', env') <- stp (a, env)
        return (Pow a' (Num b), env')
    stp (Pow a b, env) = do (b', env') <- stp (b, env)
      return (Pow a b', env')

                                                                                                --Operadores Logicos
    --Ver si se reducen a un valor, no solo a numeros
    -- igualdad
    stp (Eq (Num a) (Num b), env) = Just (Bool (a = b), env) 
    stp (Eq a (Num b), env) = do (a', env') <- stp (a, env)
        return (Pow a' (Num b), env')
    stp (Eq a b, env) = do (b', env') <- stp (a, env)
        return (Eq a' b, env')

    -- Menor que
    stp (Eq (Num a) (Num b), env) = Just (Bool (a < b), env)
    stp (Eq a (Num b), env) = do (a', env') <- stp (a, env)
        return (Pow a' (Num b), env')
    stp (Eq a b, env) = do (b', env') <- stp (a, env)
        return (Eq a' b, env')

    -- Mayor que
    stp (Eq (Num a) (Num b), env) = Just (Bool (a < b), env) 
    stp (Eq a (Num b), env) = do (a', env') <- stp (a, env)
        return (Pow a' (Num b), env')
    stp (Eq a b, env) = do (b', env') <- stp (a, env)
        return (Eq a' b, env')
    --Menor igual que 
    --Mayor igual que

                                                                                                --Funciones de listas
    --Head (lista)
    stp (Head (a:xs),env) = do (a',env') <- stp (a,env)
        return (Head (a':xs),env') 

    --Tail (lista)
    stp (Tail (c:xs), env) =
        | xs != [] == Nothing -- ??
        | otherwise = (xs',env') <- stp (xs,env)
        return (Tail (c:xs')) -- ??

                                                                        --Pares Ordenados y funciones de pares ordenados
    --Pares ordenados / agregar lo de si ya es valor en el lenguje
    stp (Pair (a b), env) = 
        | isValue a && isValue b = Nothing -- Ver que ya no se reduce tanto
        | otherwise = Just (Pair (a,b),env)
    stp (Pair (a b), env) = do (b', env') <- stp (b env)
        return (Pair (a b'), env')
    stp (Pair (a b), env) = do (a', env') <- stp (a,env)
        return (Pair (a' b, env'))

    --Primer elemento
    stp (Fst (isValue a ), env) = Just (Fst a,env)
    stp (Fst (a),env) = do (a', env') <- stp (a,env)
        return (Fst (a'),env')

    --Segundo elemento
    stp (Snd (isValue b ), env) = Just (Snd b,env)
    stp (Snd (b),env) = do (b', env') <- stp (b,env)
        return (Snd (b'),env')

                                                                            -- Condicionales
    -- If (cond, cero y varidicos es azucar)
    stp (If ( a b c), env) = Just (If b)
    stp ()   
    



