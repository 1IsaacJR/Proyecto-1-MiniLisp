{
module Lexer where
}

%wrapper "basic"

$digit   = 0-9
$alpha   = [a-zA-Z]
$alnum   = [a-zA-Z0-9_]

tokens :-

$white+                        ;
"--".*                         ;
"("                            { \_ -> TOKENPARENTESISIZQUIERDO }
")"                            { \_ -> TOKENPARENTESISDERECHO }
"["                            { \_ -> TOKENCORCHETEIZQUIERDO }
"]"                            { \_ -> TOKENCORCHETEDERECHO }
","                            { \_ -> TOKENCOMA }

"let"                          { \_ -> TOKENLET }
"letrec"                       { \_ -> TOKENLETREC }
"let*"                         { \_ -> TOKENLETESTRELLA }
"if"                           { \_ -> TOKENIF }
"if0"                          { \_ -> TOKENIFCERO }
"cond"                         { \_ -> TOKENCOND }
"else"                         { \_ -> TOKENELSE }
"lambda"                       { \_ -> TOKENLAMBDA }
"fst"                          { \_ -> TOKENPRIMERO }
"snd"                          { \_ -> TOKENSEGUNDO }
"head"                         { \_ -> TOKENCABEZA }
"tail"                         { \_ -> TOKENCOLA }
"add1"                         { \_ -> TOKENMASUNO }
"sub1"                         { \_ -> TOKENMENOSUNO }
"sqrt"                         { \_ -> TOKENRAIZCUADRADA }
"expt"                         { \_ -> TOKENPOTENCIA }

"+"                            { \_ -> TOKENSUMA }
"-"                            { \_ -> TOKENRESTA }
"*"                            { \_ -> TOKENMULTIPLICACION }
"/"                            { \_ -> TOKENDIVISION }
"="                            { \_ -> TOKENIGUAL }
"<"                            { \_ -> TOKENMENORQUE }
">"                            { \_ -> TOKENMAYORQUE }
"<="                           { \_ -> TOKENMENORIGUALQUE }
">="                           { \_ -> TOKENMAYORIGUALQUE }
"!="                           { \_ -> TOKENDIFERENTEQUE }

"#t"                           { \_ -> TOKENBOOLEANO True }
"#f"                           { \_ -> TOKENBOOLEANO False }

$digit+                        { \s -> TOKENENTERO (read s) }
$alpha($alnum)*                { \s -> TOKENIDENTIFICADOR s }

{
data Token
    = TOKENPARENTESISIZQUIERDO | TOKENPARENTESISDERECHO
    | TOKENCORCHETEIZQUIERDO | TOKENCORCHETEDERECHO | TOKENCOMA
    | TOKENLET | TOKENLETREC | TOKENLETESTRELLA
    | TOKENIF | TOKENIFCERO | TOKENCOND | TOKENELSE
    | TOKENLAMBDA
    | TOKENPRIMERO | TOKENSEGUNDO
    | TOKENCABEZA | TOKENCOLA
    | TOKENMASUNO | TOKENMENOSUNO | TOKENRAIZCUADRADA | TOKENPOTENCIA
    | TOKENSUMA | TOKENRESTA | TOKENMULTIPLICACION | TOKENDIVISION
    | TOKENIGUAL | TOKENMENORQUE | TOKENMAYORQUE
    | TOKENMENORIGUALQUE | TOKENMAYORIGUALQUE | TOKENDIFERENTEQUE
    | TOKENBOOLEANO Bool
    | TOKENENTERO Int
    | TOKENIDENTIFICADOR String
    deriving (Show, Eq)
}
