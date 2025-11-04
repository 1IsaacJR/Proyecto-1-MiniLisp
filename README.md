# Proyecto-1-MiniLisp

Implementación de un intérprete de Lisp en Haskell con evaluación small step
SASA  --(Lexer/Parser)-->  AST de superficie (SASA)
  └── Desugar.desugar  -->  AST núcleo (ASA)
         └── Desugar.desugarV  -->  Forma evaluable (ASAValues)
                └── Interprete.interp []  -->  Valor final


Estructura:

MiniLisp/
├── ASA.hs            -- AST núcleo del lenguaje
├── ASAValues.hs      -- Forma evaluable (valores + nodos runtime)
├── Desugar.hs        -- SASA -> ASA  y  ASA -> ASAValues
├── Interprete.hs     -- Small-step 'stp' + 'interp' (eval hasta valor)
├── Lexer.x           -- (opcional) alex
├── Parser.y          -- (opcional) happy
├── SASA.hs           -- AST de superficie (si usas parser)
├── Main.hs           -- REPL (usa Lexer/Parser/SASA)
└── Test.hs           -- Pruebas rápidas sin lexer/parser


Lenguaje:

Valores y literales

Num, Bool, Nil, listas (List [...]) y pares (Pair a b).

Aritmética y unarios

Binarias: Add, Sub, Mul, Div

Unarios: Add1, Sub1, Sqrt (piso), Expt

Comparadores y booleanos

Eq, Neq, Lt, Gt, Leq, Geq

Lógicos: And, Or, Not (con short-circuit)

Estructuras

Pares: Pair, Fst, Snd

Listas: List, Head, Tail, Nil

Asignaciones / Ámbito

Let — paralelo (todas las RHS en el entorno original)

LetStar — secuencial (cada binding ve a los anteriores)

LetRec — recursivo (forma de sintaxis disponible)

Control de flujo

If (condición booleana) → IfV en valores

If0 (prueba numérica contra 0) → If0V

Cond (se desazucara a cascada de If en Desugar)

Funciones

Lambda [params] body y App f [args]

Cierres (closures) con entorno léxico: ClosureV

Semántica

CBV: primero se evalúa la función y luego los argumentos, de izquierda a derecha.

Short-circuit en And/Or.

Let paralelo: las RHS no ven variables definidas en el mismo let.

Let*: secuencial por desazucarado a lambdas anidadas.

Cond: desazucarado a if anidados (requiere else).

Compilar/Ejecutar

Opción A: pruebas sin lexer/parser

ghc -O2 Test.hs -o test && ./test

runghc Test.hs

Opción B: REPL con lexer/parser (opcional)

Genera el lexer/parser si corresponde, compila y corre:

ghc -O2 Main.hs -o repl
./repl

Notas sobre diseño:
Let: paralelo ⇒ App (Lambda vars body) vals.

Let*: secuencial ⇒ lambdas anidadas.

LetRec: forma disponible; el atado recursivo completo depende de la estrategia elegida (fix-point/combinador o entorno auto-referencial).

Desugar.hs:

desugar :: SASA -> ASA (quita azúcar: let, let*, cond, etc.)

desugarV :: ASA -> ASAValues (mapea al lenguaje de valores con IfV/If0V, LambdaV, AppV, …)

Interprete.hs:

stp :: (ASAValues, Env) -> Maybe (ASAValues, Env) — paso pequeño

interp :: ASAValues -> Env -> ASAValues — itera hasta valor


