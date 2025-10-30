-- Test.hs
module Main where

import ASA
import ASAValues
import Desugar (desugar, desugarV)
import Interprete (interp)

type Test = (String, ASA, ASAValues)

-- corre: ASA -> valor final (usamos entorno vacío)
run :: ASA -> ASAValues
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

tests :: [Test]
tests =
  [ -- Aritmética y unarios (solo binarios anidados)
    ("suma",      Add (Add (Add (Num 1) (Num 2)) (Num 3)) (Num 4) , NumV 10)
  , ("mul",       Mul (Mul (Num 3) (Num 4)) (Num 5)               , NumV 60)
  , ("div",       Div (Div (Num 120) (Num 3)) (Num 2)             , NumV 20)
  , ("add1",      Add1 (Num 7)                                     , NumV 8)
  , ("sub1",      Sub1 (Num 0)                                     , NumV (-1))
  , ("expt",      Expt (Num 2) (Num 10)                            , NumV 1024)
  , ("sqrt",      Sqrt (Num 9)                                     , NumV 3)

    -- Comparadores
  , ("eq",        Eq  (Num 4) (Num 4)                              , BoolV True)
  , ("lt",        Lt  (Num 2) (Num 3)                              , BoolV True)
  , ("gt_false",  Gt  (Num 3) (Num 3)                              , BoolV False)
  , ("leq_true",  Leq (Num 3) (Num 3)                              , BoolV True)
  , ("geq_true",  Geq (Num 4) (Num 3)                              , BoolV True)
  , ("neq_true",  Neq (Num 4) (Num 5)                              , BoolV True)

    -- let paralelo: let ((x 3) (y (+ x 1))) (+ x y)
    -- Ojo: en let paralelo, y no ve el "x nuevo" (usa env original). Tu desugar lo hace:
  , ("let_paralelo",
      Let [ ("x", Num 3)
          , ("y", Add (Var "x") (Num 1))
          ]
          (Add (Var "x") (Var "y"))
    , NumV 7
    )

    -- let* secuencial
  , ("let_star",
      LetStar [ ("x", Num 3)
              , ("y", Add (Var "x") (Num 1))
              ]
              (Add (Var "x") (Var "y"))
    , NumV 7
    )

    -- if booleano
  , ("if_true",   If (Bool True)  (Num 42) (Num 0)                 , NumV 42)
  , ("if_false",  If (Bool False) (Num 42) (Num 0)                 , NumV 0)

    -- if0 numérico
  , ("if0_zero",  If0 (Num 0) (Num 1) (Num 2)                      , NumV 1)
  , ("if0_non0",  If0 (Num 5) (Num 1) (Num 2)                      , NumV 2)

    -- cond con else (tu desugar lo transforma a ifs)
  , ("cond_equiv",
      Cond [ (Lt (Num 0) (Num 1), Num 42) ] (Just (Num 0))
    , NumV 42
    )

    -- pares y listas
  , ("pair_fst",  Fst (Pair (Num 1) (Bool True))                   , NumV 1)
  , ("pair_snd",  Snd (Pair (Num 3) (Num 5))                       , NumV 5)
  , ("head_list", Head (List [Num 1, Num 2, Num 3])                , NumV 1)
  , ("tail_list", Tail (List [Num 1, Num 2, Num 3])                , ListV [NumV 2, NumV 3])

    -- lambda + aplicación multiarg (tu core la mantiene como multi-arg y el intérprete aplica CBV)
  , ("lambda_app3",
      App (Lambda ["x","y","z"] (Add (Add (Var "x") (Var "y")) (Var "z")))
          [Num 1, Num 2, Num 3]
    , NumV 6
    )
  ]

main :: IO ()
main = do
  putStrLn "=== MiniLisp Test Runner ==="
  oks <- mapM expect tests
  let total = length oks
      passed = length (filter id oks)
  putStrLn "----------------------------"
  putStrLn ("Passed: " ++ show passed ++ " / " ++ show total)
  if passed == total
    then putStrLn "All tests PASS"
    else putStrLn "Some tests FAILED"
