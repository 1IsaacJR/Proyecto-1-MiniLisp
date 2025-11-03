module Main where

import SASA
import ASA
import ASAValues
import Desugar (desugar, desugarV)
import Interprete (interp)

-- Tipo de prueba: nombre, programa SASA, valor esperado ASAValues
type Test = (String, SASA, ASAValues)

-- corre: SASA -> valor final (pasando por desugar, desugarV e interp)
run :: SASA -> ASAValues
run e = interp (desugarV (desugar e)) []

expect :: Test -> IO Bool
expect (name, prog, want) = do
  let got = run prog
  if got == want
    then putStrLn ("[PASS] " ++ name) >> pure True
    else do
      putStrLn ("[FAIL] " ++ name)
      putStrLn ("  got : " ++ show got)
      putStrLn ("  want: " ++ show want)
      pure False

tests :: [(String, SASA, ASAValues)]
tests =
  [ -- Operaciones básicas
    ("suma",     AddS [NumS 1, NumS 2, NumS 3, NumS 4], NumV 10)
  , ("mul",      MulS [NumS 3, NumS 4, NumS 5], NumV 60)
  , ("div",      DivS [NumS 120, NumS 3, NumS 2], NumV 20)
  , ("eq",       EqS  [NumS 4, NumS 4], BoolV True)
  , ("lt",       LtS  [NumS 2, NumS 3], BoolV True)
  , ("gt_false", GtS [NumS 3, NumS 3], BoolV False)
  , ("leq_true", LeqS [NumS 3, NumS 3], BoolV True)
  , ("geq_true", GeqS [NumS 4, NumS 3], BoolV True)
  , ("neq_true", NeqS [NumS 4, NumS 5], BoolV True)

  -- Operaciones unarias
  , ("add1", Add1S (NumS 2), NumV 3)
  , ("sub1", Sub1S (NumS 2), NumV 1)
  , ("sqrt", SqrtS (NumS 9), NumV 3)
  , ("expt", ExptS (NumS 2) (NumS 3), NumV 8)

  -- Condicionales
  , ("if", IfS (BoolS True) (NumS 1) (NumS 0), NumV 1)
  , ("if0", If0S (NumS 0) (NumS 42) (NumS 99), NumV 42)
  , ("cond_with_else", CondS [(LtS [NumS 1, NumS 2], NumS 42)] (Just (NumS 0)), NumV 42)

  -- Lets
  , ("let_parallel", LetS [("x", NumS 3), ("y", NumS 4)] (AddS [VarS "x", VarS "y"]), NumV 7)
  , ("let_empty", LetS [] (NumS 42), NumV 42)
  , ("let_star_seq", LetStarS [("x", NumS 3), ("y", AddS [VarS "x", NumS 1])] (AddS [VarS "x", VarS "y"]), NumV 7)

  -- Funciones y aplicación
  , ("lambda", LambdaS ["x"] (Add1S (VarS "x")), ClosureV ["x"] (Add1V (VarV "x")) [])
  , ("app_simple", AppS (LambdaS ["x"] (AddS [VarS "x", NumS 1])) [NumS 5], NumV 6)
  , ("app_multi", AppS (LambdaS ["x","y"] (AddS [VarS "x", VarS "y"])) [NumS 3, NumS 4], NumV 7)

  -- Pares (usando PairS de SASA, no Pair de ASA)
  , ("pair_fst",  FstS (PairS (NumS 1) (BoolS True)), NumV 1)
  , ("pair_snd",  SndS (PairS (NumS 3) (NumS 5)), NumV 5)

  -- Listas (usando ListS de SASA, no List de ASA)
  , ("head_list", HeadS (ListS [NumS 1, NumS 2, NumS 3]), NumV 1)
  , ("tail_list", TailS (ListS [NumS 1, NumS 2, NumS 3]), ListV [NumV 2, NumV 3])
  , ("nil_list", NilS, NilV)

  -- Funciones recursivas (las 5 funciones requeridas)
  , ("suma_n_naturales", LetRecS [("suma", LambdaS ["n"] (If0S (VarS "n") (NumS 0) (AddS [VarS "n", AppS (VarS "suma") [Sub1S (VarS "n")]])))] (AppS (VarS "suma") [NumS 5]), NumV 15)

  , ("factorial", LetRecS [("fact", LambdaS ["n"] (If0S (VarS "n") (NumS 1) (MulS [VarS "n", AppS (VarS "fact") [Sub1S (VarS "n")]])))] (AppS (VarS "fact") [NumS 5]), NumV 120)

  , ("fibonacci", LetRecS [("fib", LambdaS ["n"] (CondS [(EqS [VarS "n", NumS 0], NumS 0), (EqS [VarS "n", NumS 1], NumS 1)] (Just (AddS [AppS (VarS "fib") [Sub1S (VarS "n")], AppS (VarS "fib") [SubS [VarS "n", NumS 2]]]))))] (AppS (VarS "fib") [NumS 6]), NumV 8)

-- Map y Filter (versiones simplificadas)
  , ("map_simple", LetS [("doble", LambdaS ["x"] (MulS [VarS "x", NumS 2]))] (AppS (VarS "doble") [NumS 3]), NumV 6)

  , ("filter_simple", LetS [("mayorQue2", LambdaS ["x"] (GtS [VarS "x", NumS 2]))] (AppS (VarS "mayorQue2") [NumS 3]), BoolV True)
  ]

main :: IO ()
main = do
  putStrLn "=== MiniLisp Test Runner (SASA → ASA → ASAValues) ==="
  oks <- mapM expect tests
  let total = length oks
      passed = length (filter id oks)
  putStrLn "-------------------------------------------"
  putStrLn ("Passed: " ++ show passed ++ " / " ++ show total)
  if passed == total
    then putStrLn "All tests PASS"
    else putStrLn "Some tests FAILED"