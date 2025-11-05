module Main where

import Lexer
import Parser
import SASA
import ASA
import ASAValues
import Desugar (desugar, desugarV)
import Interprete (interp,stpStrict)
import Control.Exception (catch, SomeException)

main :: IO ()
main = do
    putStrLn "Introduce una expresión SASA (escribe 'salir' o Ctrl+C para terminar)."
    loop

loop :: IO ()
loop = do
    putStr ">>> "
    line <- getLineSafe
    case line of
        Nothing -> putStrLn "\nAdiós "
        Just "salir" -> putStrLn "Saliendo del intérprete..."
        Just input -> do
            procesar input
            loop
            
getLineSafe :: IO (Maybe String)
getLineSafe = catch (Just <$> getLine) handleEOF
  where
    handleEOF :: SomeException -> IO (Maybe String)
    handleEOF _ = return Nothing


procesar :: String -> IO ()
procesar input = catch (do
    -- Paso 1: Lexer
    let tokens = alexScanTokens input
    putStrLn "\n--- Paso 1: Lexer (tokens) ---"
    print tokens

    -- Paso 2: Parser
    let sasa = parse tokens
    putStrLn "\n--- Paso 2: Parser (SASA) ---"
    print sasa

    -- Paso 3: Desugar
    let asa = desugar sasa
    putStrLn "\n--- Paso 3: Desugar (ASA inicial) ---"
    print asa

    -- Paso 4: DesugarV
    let asaV = desugarV asa
    putStrLn "\n--- Paso 4: DesugarV (transformación adicional) ---"
    print asaV

    -- Paso 5: Interprete
    let valor = interp asaV []
    putStrLn "\n--- Paso 5: Interprete (valor final) ---"
    print valor
    putStrLn ""
    ) manejarErrores

manejarErrores :: SomeException -> IO ()
manejarErrores e = do
    putStrLn "\n[Error capturado]"
    print e
    putStrLn ""


