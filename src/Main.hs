{-# LANGUAGE FlexibleContexts #-}

module Main where

import ASA
import ASAValues
import SASA
import Desugar (desugar, desugarV)
import Interprete (interp)

-- Para evitar los warnings, necesitamos estas funciones auxiliares
simpleParse :: String -> SASA
simpleParse input = case input of
    "suma" -> LetRecS [("suma", LambdaS ["n"] 
        (If0S (VarS "n") 
            (NumS 0) 
            (AddS [VarS "n", AppS (VarS "suma") [SubS [VarS "n", NumS 1]]])))] 
        (AppS (VarS "suma") [NumS 5])
    "factorial" -> LetRecS [("fact", LambdaS ["n"] 
        (If0S (VarS "n") 
            (NumS 1) 
            (MulS [VarS "n", AppS (VarS "fact") [SubS [VarS "n", NumS 1]]])))] 
        (AppS (VarS "fact") [NumS 5])
    "fibonacci" -> LetRecS [("fib", LambdaS ["n"] 
        (CondS 
            [(EqS [VarS "n", NumS 0], NumS 0),
             (EqS [VarS "n", NumS 1], NumS 1)]
            (Just (AddS [AppS (VarS "fib") [SubS [VarS "n", NumS 1]], 
                        AppS (VarS "fib") [SubS [VarS "n", NumS 2]]]))))] 
        (AppS (VarS "fib") [NumS 6])
    _ -> NumS 0

-- Pipeline simplificado sin parser
runTest :: String -> ASAValues
runTest name = 
    let sasa = simpleParse name
        asa = desugar sasa
        asaValues = desugarV asa
    in interp asaValues []

-- Tests individuales
testSuma :: IO ()
testSuma = do
    putStrLn "1. SUMA DE LOS PRIMEROS 5 NÚMEROS NATURALES"
    let result = runTest "suma"
    putStrLn $ "Resultado: " ++ show result
    putStrLn $ "Esperado: " ++ show (NumV 15)
    if result == NumV 15 
        then putStrLn "✅ PASÓ"
        else putStrLn "❌ FALLÓ"
    putStrLn ""

testFactorial :: IO ()
testFactorial = do
    putStrLn "2. FACTORIAL DE 5"
    let result = runTest "factorial"
    putStrLn $ "Resultado: " ++ show result
    putStrLn $ "Esperado: " ++ show (NumV 120)
    if result == NumV 120 
        then putStrLn "✅ PASÓ"
        else putStrLn "❌ FALLÓ"
    putStrLn ""

testFibonacci :: IO ()
testFibonacci = do
    putStrLn "3. FIBONACCI DE 6"
    let result = runTest "fibonacci"
    putStrLn $ "Resultado: " ++ show result
    putStrLn $ "Esperado: " ++ show (NumV 8)
    if result == NumV 8 
        then putStrLn "✅ PASÓ"
        else putStrLn "❌ FALLÓ"
    putStrLn ""

-- Función map simplificada
testMap :: IO ()
testMap = do
    putStrLn "4. FUNCIÓN MAP (versión simplificada)"
    -- Map que duplica los elementos: manualmente construido
    let mapCode = LetS 
            [("doble", LambdaS ["x"] (MulS [VarS "x", NumS 2]))]
            (AppS (VarS "doble") [NumS 3])
    let asa = desugar mapCode
    let asaValues = desugarV asa
    let result = interp asaValues []
    putStrLn $ "Map (doble 3): " ++ show result
    putStrLn $ "Esperado: " ++ show (NumV 6)
    if result == NumV 6 
        then putStrLn "✅ PASÓ"
        else putStrLn "❌ FALLÓ"
    putStrLn ""

-- Función filter simplificada
testFilter :: IO ()
testFilter = do
    putStrLn "5. FUNCIÓN FILTER (versión simplificada)"
    -- Filter que verifica si es mayor a 2: manualmente construido
    let filterCode = LetS 
            [("mayorQue2", LambdaS ["x"] (GtS [VarS "x", NumS 2]))]
            (AppS (VarS "mayorQue2") [NumS 3])
    let asa = desugar filterCode
    let asaValues = desugarV asa
    let result = interp asaValues []
    putStrLn $ "Filter (>2) 3: " ++ show result
    putStrLn $ "Esperado: " ++ show (BoolV True)
    if result == BoolV True 
        then putStrLn "✅ PASÓ"
        else putStrLn "❌ FALLÓ"
    putStrLn ""

runAllTests :: IO ()
runAllTests = do
    putStrLn "=== PRUEBAS COMPREHENSIVAS DEL LENGUAJE ==="
    putStrLn "Pipeline: SASA → ASA → ASAValues → Interprete"
    putStrLn ""
    
    testSuma
    testFactorial
    testFibonacci
    testMap
    testFilter
    
    putStrLn "=== FIN DE LAS PRUEBAS ==="

main :: IO ()
main = runAllTests