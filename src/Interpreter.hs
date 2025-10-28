-- src/Interpreter.hs
module Interpreter
  ( evalASA   -- evaluar directamente un ASA
  , evalSASA  -- evaluar una SASA (usa desugar)
  ) where

import qualified ASA as A        -- AST desazucarado y tipos de Value/Env
import qualified SASA as S       -- AST superficial (solo para la función evalSASA)
import Desugar (desugar)      -- transforma SASA -> ASA
import Data.Maybe (fromMaybe)

-- Evaluador (big-step, ansioso), devuelve un A.Value (definido en ASA).
-- Firma principal:
evalASA :: A.ASA -> A.Env -> A.Value
evalASA expr env = eval expr env

-- Evaluar desde SASA directamente (usa desugar)
evalSASA :: S.SASA -> A.Env -> A.Value
evalSASA s env = eval (desugar s) env

-- ===================================================================
-- Helpers: convertir/consultar entorno (A.Env está definido en el módulo ASA)
-- ===================================================================

lookupEnv :: String -> A.Env -> Maybe A.Value
lookupEnv = lookup

-- construir entorno a partir de listas de nombres y valores
extendEnv :: [(String, A.Value)] -> A.Env -> A.Env
extendEnv bindings env = bindings ++ env

-- ===================================================================
-- Evaluador (implementación eager, lexical scope)
-- ===================================================================

eval :: A.ASA -> A.Env -> A.Value

-- Literales y variables
eval (A.Num n)    _   = A.NumV n
eval (A.Bool b)   _   = A.BoolV b
eval (A.Var x)    env = fromMaybe (A.ErrorV $ "Variable no definida: " ++ x) (lookupEnv x env)

-- Lambda: crea una cerradura (closure) con el entorno léxico actual
eval (A.Lambda params body) env = A.Closure params body env

-- Aplicación: evaluar función y argumentos (ansioso), luego aplicar
eval (A.App f args) env =
  case eval f env of
    A.Closure params body envClosure ->
      if length params /= length args
        then A.ErrorV $ "Aridad incorrecta: función espera " ++ show (length params)
                       ++ " argumentos pero se dieron " ++ show (length args)
        else
          let argVals = map (`eval` env) args                         -- eager: evaluar args en el entorno call-site
              newBindings = zip params argVals
              newEnv = extendEnv newBindings envClosure               -- alcance léxico: usar env del closure
          in eval body newEnv
    v -> A.ErrorV $ "Intento de aplicar algo no-función: " ++ show v

-- Lets
-- Let: evaluar RHS en el entorno actual (no recursivo), luego evaluar cuerpo con bindings añadidos
eval (A.Let binds body) env =
  let evaled = map (\(name, rhs) -> (name, eval rhs env)) binds
      env' = extendEnv evaled env
  in eval body env'

-- LetRec: construir entorno recursivo (cada binding puede ver las otras bindings)
eval (A.LetRec binds body) env =
  let names = map fst binds
      -- placeholder env que contiene los nombres ligados a ErrorV temporalmente
      recEnvPlace = zip names (repeat (A.ErrorV "<placeholder>")) ++ env
      -- ahora evaluar cada rhs **en el entorno recursivo** recEnvPlace
      vals = map (\(_,rhs) -> eval rhs recEnvPlace) binds
      envRec = zip names vals ++ env
  in eval body envRec

-- LetStar: secuencial (cada binding ve los anteriores)
eval (A.LetStar [] body) env = eval body env
eval (A.LetStar ((x,r):xs) body) env =
  let v = eval r env
      env' = (x, v) : env
  in eval (A.LetStar xs body) env'

-- Operadores aritméticos binarios (trabajan con enteros, resultado Int)
eval (A.Add a b) env =
  case (eval a env, eval b env) of
    (A.NumV x, A.NumV y) -> A.NumV (x + y)
    (x, y) -> A.ErrorV $ "Type error in +: " ++ show x ++ " , " ++ show y

eval (A.Sub a b) env =
  case (eval a env, eval b env) of
    (A.NumV x, A.NumV y) -> A.NumV (x - y)
    (x, y) -> A.ErrorV $ "Type error in -: " ++ show x ++ " , " ++ show y

eval (A.Mul a b) env =
  case (eval a env, eval b env) of
    (A.NumV x, A.NumV y) -> A.NumV (x * y)
    (x, y) -> A.ErrorV $ "Type error in *: " ++ show x ++ " , " ++ show y

eval (A.Div a b) env =
  case (eval a env, eval b env) of
    (A.NumV _, A.NumV 0) -> A.ErrorV "Division por cero"
    (A.NumV x, A.NumV y) -> A.NumV (x `div` y)
    (x, y) -> A.ErrorV $ "Type error in /: " ++ show x ++ " , " ++ show y

-- Exponent (expt) and unary ops
eval (A.Expt a b) env =
  case (eval a env, eval b env) of
    (A.NumV x, A.NumV y) ->
      if y < 0
        then A.ErrorV "Exponente negativo no soportado (enteros)"
        else A.NumV (x ^ y)
    (x, y) -> A.ErrorV $ "Type error in expt: " ++ show x ++ " , " ++ show y

eval (A.Add1 a) env =
  case eval a env of
    A.NumV x -> A.NumV (x + 1)
    x -> A.ErrorV $ "Type error in add1: " ++ show x

eval (A.Sub1 a) env =
  case eval a env of
    A.NumV x -> A.NumV (x - 1)
    x -> A.ErrorV $ "Type error in sub1: " ++ show x

-- sqrt: para enteros, usamos truncamiento (simple ejemplo)
eval (A.Sqrt a) env =
  case eval a env of
    A.NumV x | x >= 0 -> A.NumV (floor (sqrt (fromIntegral x)))
    A.NumV _ -> A.ErrorV "sqrt de negativo"
    x -> A.ErrorV $ "Type error in sqrt: " ++ show x

-- Comparaciones (retornan BoolV)
eval (A.Eq a b) env =
  case (eval a env, eval b env) of
    (A.NumV x, A.NumV y) -> A.BoolV (x == y)
    (A.BoolV x, A.BoolV y) -> A.BoolV (x == y)
    (x,y) -> A.ErrorV $ "Type error in =: " ++ show x ++ " , " ++ show y

eval (A.Lt a b) env =
  case (eval a env, eval b env) of
    (A.NumV x, A.NumV y) -> A.BoolV (x < y)
    (x,y) -> A.ErrorV $ "Type error in <: " ++ show x ++ " , " ++ show y

eval (A.Gt a b) env =
  case (eval a env, eval b env) of
    (A.NumV x, A.NumV y) -> A.BoolV (x > y)
    (x,y) -> A.ErrorV $ "Type error in >: " ++ show x ++ " , " ++ show y

eval (A.Leq a b) env =
  case (eval a env, eval b env) of
    (A.NumV x, A.NumV y) -> A.BoolV (x <= y)
    (x,y) -> A.ErrorV $ "Type error in <=: " ++ show x ++ " , " ++ show y

eval (A.Geq a b) env =
  case (eval a env, eval b env) of
    (A.NumV x, A.NumV y) -> A.BoolV (x >= y)
    (x,y) -> A.ErrorV $ "Type error in >=: " ++ show x ++ " , " ++ show y

eval (A.Neq a b) env =
  case (eval a env, eval b env) of
    (A.NumV x, A.NumV y) -> A.BoolV (x /= y)
    (A.BoolV x, A.BoolV y) -> A.BoolV (x /= y)
    (x,y) -> A.ErrorV $ "Type error in !=: " ++ show x ++ " , " ++ show y

-- If / If0
eval (A.If cond t e) env =
  case eval cond env of
    A.BoolV True  -> eval t env
    A.BoolV False -> eval e env
    x -> A.ErrorV $ "If: condición no booleana: " ++ show x

eval (A.If0 cond t e) env =
  case eval cond env of
    A.NumV 0 -> eval t env
    A.NumV _ -> eval e env
    x -> A.ErrorV $ "If0: condición no numérica: " ++ show x

-- Pairs
eval (A.Pair a b) env =
  let va = eval a env
      vb = eval b env
  in case (va, vb) of
       (A.ErrorV msg, _) -> A.ErrorV msg
       (_, A.ErrorV msg) -> A.ErrorV msg
       _ -> A.PairV va vb

eval (A.Fst p) env =
  case eval p env of
    A.PairV x _ -> x
    x -> A.ErrorV $ "Fst: no es par: " ++ show x

eval (A.Snd p) env =
  case eval p env of
    A.PairV _ y -> y
    x -> A.ErrorV $ "Snd: no es par: " ++ show x

-- Lists
eval (A.List xs) env =
  let vs = map (`eval` env) xs
  in if any isErr vs then head (filter isErr vs) else A.ListV vs
  where isErr (A.ErrorV _) = True; isErr _ = False

eval (A.Head l) env =
  case eval l env of
    A.ListV (x:_) -> x
    A.ListV []    -> A.ErrorV "Head de lista vacía"
    x -> A.ErrorV $ "Head: no es lista: " ++ show x

eval (A.Tail l) env =
  case eval l env of
    A.ListV (_:xs) -> A.ListV xs
    A.ListV []     -> A.ErrorV "Tail de lista vacía"
    x -> A.ErrorV $ "Tail: no es lista: " ++ show x

-- Cond (lista de pares) -> desugar tipico a if anidado: evaluamos aquí secuencialmente
eval (A.Cond [] Nothing) _ = A.ErrorV "cond vacío sin else"
eval (A.Cond [] (Just alt)) env = eval alt env
eval (A.Cond ((c,e):cs) mElse) env =
  case eval c env of
    A.BoolV True -> eval e env
    A.BoolV False -> eval (A.Cond cs mElse) env
    x -> A.ErrorV $ "Cond: condición no booleana: " ++ show x

-- Caso por defecto (no reconocido)
eval other _ = A.ErrorV $ "Construcción no soportada por el intérprete (eval): " ++ show other
