module Main where

import SASA
import ASA
import ASAValues
import Desugar
import Interprete


-- Función para mostrar los resultados de manera legible
saca :: ASAValues -> String
saca (NumV n) = show n
saca (BoolV True) = "#t"
saca (BoolV False) = "#f"
saca (ClosureV _ _ _) = "#<procedure>"
saca _ = "#<unknown>"

-- Función que ejecuta todo el flujo: SASA → ASA → ASAValues → Intérprete
runExample :: SASA -> IO ()
runExample exprS = do
    let exprA = desugar exprS         -- SASA → ASA
    let exprV = desugarV exprA        -- ASA → ASAValues
    putStrLn "\n=== Expresión desazucarada (ASA) ==="
    print exprA
    putStrLn "\n=== Expresión en ASAValues ==="
    print exprV
    putStrLn "\n=== Resultado de la evaluación ==="
    putStrLn (saca (eval exprV []))   -- ← Aquí el cambio

-- Evaluador auxiliar para ejecutar stp varias veces hasta llegar a un valor
eval :: ASAValues -> Env -> ASAValues
eval v env =
    case stp (v, env) of
        Just (v', env') -> eval v' env'
        Nothing -> v

main :: IO ()
main = do
    putStrLn "=== Pruebas de operaciones básicas ==="

    let expr1 = AddS [NumS 10, NumS 5]
    runExample expr1  -- Esperado: 15

    let expr2 = SubS [NumS 10, NumS 3]
    runExample expr2  -- Esperado: 7

    let expr3 = MulS [NumS 4, NumS 6]
    runExample expr3  -- Esperado: 24

    let expr4 = DivS [NumS 20, NumS 5]
    runExample expr4  -- Esperado: 4

    let expr5 = AddS [NumS 2, SubS [NumS 10, NumS 3], MulS [NumS 2, NumS 3]]
    runExample expr5  -- Esperado: 15

     -- Suma variádica
    let addExample = AddS [NumS 2, NumS 3, NumS 4]  -- 2 + 3 + 4 = 9
    runExample addExample

    -- Resta variádica
    let subExample = SubS [NumS 10, NumS 3, NumS 2] -- 10 - 3 - 2 = 5
    runExample subExample

    -- Multiplicación variádica
    let mulExample = MulS [NumS 2, NumS 3, NumS 4]  -- 2 * 3 * 4 = 24
    runExample mulExample

    -- División variádica
    let divExample = DivS [NumS 100, NumS 5, NumS 2] -- 100 / 5 / 2 = 10
    runExample divExample

    -- Incremento y decremento
    let add1Example = Add1S (NumS 7)  -- 7 + 1 = 8
    runExample add1Example

    let sub1Example = Sub1S (NumS 7)  -- 7 - 1 = 6
    runExample sub1Example

    -- Raíz cuadrada
    let sqrtExample = SqrtS (NumS 16)  -- sqrt(16) = 4
    runExample sqrtExample

    -- Potencia
    let exptExample = ExptS (NumS 2) (NumS 3)  -- 2^3 = 8
    runExample exptExample

    -- Combinación de varias operaciones
    let comboExample = AddS [MulS [NumS 2, NumS 3], ExptS (NumS 2) (NumS 3), Sub1S (NumS 5)] 
    -- (2*3) + (2^3) + (5-1) = 6 + 8 + 4 = 18
    runExample comboExample
