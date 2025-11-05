{
module Parser where
import SASA
import Lexer    -- token type comes from aquí

-- El parser generado devolverá un SASA
}

%name parse
%tokentype { Token }
%error { parseError }

%token
  TOKENPARENTESISIZQUIERDO { TOKENPARENTESISIZQUIERDO }
  TOKENPARENTESISDERECHO   { TOKENPARENTESISDERECHO }
  TOKENCORCHETEIZQUIERDO   { TOKENCORCHETEIZQUIERDO }
  TOKENCORCHETEDERECHO     { TOKENCORCHETEDERECHO }
  TOKENCOMA                { TOKENCOMA }

  TOKENLET                 { TOKENLET }
  TOKENLETREC              { TOKENLETREC }
  TOKENLETESTRELLA         { TOKENLETESTRELLA }

  TOKENIF                  { TOKENIF }
  TOKENIFCERO              { TOKENIFCERO }
  TOKENCOND                { TOKENCOND }
  TOKENELSE                { TOKENELSE }

  TOKENLAMBDA              { TOKENLAMBDA }

  TOKENPRIMERO             { TOKENPRIMERO }
  TOKENSEGUNDO             { TOKENSEGUNDO }
  TOKENCABEZA              { TOKENCABEZA }
  TOKENCOLA                { TOKENCOLA }

  TOKENMASUNO              { TOKENMASUNO }
  TOKENMENOSUNO            { TOKENMENOSUNO }
  TOKENRAIZCUADRADA        { TOKENRAIZCUADRADA }
  TOKENPOTENCIA            { TOKENPOTENCIA }

  TOKENSUMA                { TOKENSUMA }
  TOKENRESTA               { TOKENRESTA }
  TOKENMULTIPLICACION      { TOKENMULTIPLICACION }
  TOKENDIVISION            { TOKENDIVISION }

  TOKENIGUAL               { TOKENIGUAL }
  TOKENMENORQUE            { TOKENMENORQUE }
  TOKENMAYORQUE            { TOKENMAYORQUE }
  TOKENMENORIGUALQUE       { TOKENMENORIGUALQUE }
  TOKENMAYORIGUALQUE       { TOKENMAYORIGUALQUE }
  TOKENDIFERENTEQUE        { TOKENDIFERENTEQUE }

  TOKENBOOLEANO            { TOKENBOOLEANO $$ }
  TOKENENTERO              { TOKENENTERO $$ }
  TOKENIDENTIFICADOR       { TOKENIDENTIFICADOR $$ }

%%

SASA
  : Expr                                  { $1 }
  ;

-- Expresiones SASA (numero, bool, var, formas prefijas y aplicaciones generales)
Expr
  : TOKENENTERO                            { NumS $1 }
  | TOKENBOOLEANO                          { BoolS $1 }
  | TOKENIDENTIFICADOR                     { VarS $1 }

  -- Operaciones variádicas: (+ e1 e2 ...), etc.
  | TOKENPARENTESISIZQUIERDO TOKENSUMA ExprList TOKENPARENTESISDERECHO
                                           { AddS $3 }
  | TOKENPARENTESISIZQUIERDO TOKENRESTA ExprList TOKENPARENTESISDERECHO
                                           { SubS $3 }
  | TOKENPARENTESISIZQUIERDO TOKENMULTIPLICACION ExprList TOKENPARENTESISDERECHO
                                           { MulS $3 }
  | TOKENPARENTESISIZQUIERDO TOKENDIVISION ExprList TOKENPARENTESISDERECHO
                                           { DivS $3 }
  | TOKENPARENTESISIZQUIERDO TOKENIGUAL ExprList TOKENPARENTESISDERECHO
                                           { EqS $3 }
  | TOKENPARENTESISIZQUIERDO TOKENMENORQUE ExprList TOKENPARENTESISDERECHO
                                           { LtS $3 }
  | TOKENPARENTESISIZQUIERDO TOKENMAYORQUE ExprList TOKENPARENTESISDERECHO
                                           { GtS $3 }
  | TOKENPARENTESISIZQUIERDO TOKENMENORIGUALQUE ExprList TOKENPARENTESISDERECHO
                                           { LeqS $3 }
  | TOKENPARENTESISIZQUIERDO TOKENMAYORIGUALQUE ExprList TOKENPARENTESISDERECHO
                                           { GeqS $3 }
  | TOKENPARENTESISIZQUIERDO TOKENDIFERENTEQUE ExprList TOKENPARENTESISDERECHO
                                           { NeqS $3 }

  -- Unarios y otras operaciones nativas
  | TOKENPARENTESISIZQUIERDO TOKENMASUNO Expr TOKENPARENTESISDERECHO
                                           { Add1S $3 }
  | TOKENPARENTESISIZQUIERDO TOKENMENOSUNO Expr TOKENPARENTESISDERECHO
                                           { Sub1S $3 }
  | TOKENPARENTESISIZQUIERDO TOKENRAIZCUADRADA Expr TOKENPARENTESISDERECHO
                                           { SqrtS $3 }
  | TOKENPARENTESISIZQUIERDO TOKENPOTENCIA Expr Expr TOKENPARENTESISDERECHO
                                           { ExptS $3 $4 }

 -- Aplicación general: (f arg1 arg2 ...)
  | TOKENPARENTESISIZQUIERDO Expr ExprList TOKENPARENTESISDERECHO
                                           { AppS $2 $3 }

  -- Pares, fst, snd
  | TOKENPARENTESISIZQUIERDO TOKENPRIMERO Expr TOKENPARENTESISDERECHO
                                           { FstS $3 }
  | TOKENPARENTESISIZQUIERDO TOKENSEGUNDO Expr TOKENPARENTESISDERECHO
                                           { SndS $3 }

  -- ELIMINADA: la regla conflictiva de PairS
  
  
  -- Listas literales: [e1, e2, e3]
  | TOKENCORCHETEIZQUIERDO ListElems TOKENCORCHETEDERECHO
                                           { ListS $2 }
  | TOKENCORCHETEIZQUIERDO TOKENCORCHETEDERECHO
                                           { ListS [] }

  -- Head / Tail
  | TOKENPARENTESISIZQUIERDO TOKENCABEZA Expr TOKENPARENTESISDERECHO
                                           { HeadS $3 }
  | TOKENPARENTESISIZQUIERDO TOKENCOLA Expr TOKENPARENTESISDERECHO
                                           { TailS $3 }

  -- If / If0
  | TOKENPARENTESISIZQUIERDO TOKENIF Expr Expr Expr TOKENPARENTESISDERECHO
                                           { IfS $3 $4 $5 }
  | TOKENPARENTESISIZQUIERDO TOKENIFCERO Expr Expr Expr TOKENPARENTESISDERECHO
                                           { If0S $3 $4 $5 }

  -- Cond (cond ((c1 e1) (c2 e2) ...) else)
  | TOKENPARENTESISIZQUIERDO TOKENCOND CondBranchList TOKENPARENTESISDERECHO
                                           { CondS $3 Nothing }
  | TOKENPARENTESISIZQUIERDO TOKENCOND CondBranchList TOKENELSE Expr TOKENPARENTESISDERECHO
                                           { CondS $3 (Just $5) }

  -- Let, LetRec, Let*
  | TOKENPARENTESISIZQUIERDO TOKENLET BindingList Expr TOKENPARENTESISDERECHO
                                           { LetS $3 $4 }
  | TOKENPARENTESISIZQUIERDO TOKENLETREC BindingList Expr TOKENPARENTESISDERECHO
                                           { LetRecS $3 $4 }
  | TOKENPARENTESISIZQUIERDO TOKENLETESTRELLA BindingList Expr TOKENPARENTESISDERECHO
                                           { LetStarS $3 $4 }

  -- Lambda (lambda (x y ...) body)
  | TOKENPARENTESISIZQUIERDO TOKENLAMBDA TOKENPARENTESISIZQUIERDO ParamList TOKENPARENTESISDERECHO Expr TOKENPARENTESISDERECHO
                                           { LambdaS $4 $6 }



  ;

-- Lista de expresiones (para operadores y aplicaciones)
ExprList
  :                                         { [] }
  | Expr ExprList                           { $1 : $2 }
  ;

-- Lista de parametros en lambda (identificadores)
ParamList
  :                                         { [] }
  | TOKENIDENTIFICADOR ParamList            { $1 : $2 }
  ;

-- Lista de elementos para listas literales [e1, e2, ...]
ListElems
  : Expr ListElemsRest                      { $1 : $2 }
  ;

ListElemsRest
  : TOKENCOMA ListElems                     { $2 }
  |                                         { [] }
  ;

-- Lista de bindings para let: ( (x e) (y e2) ... )
BindingList
  : TOKENPARENTESISIZQUIERDO ManyBinding TOKENPARENTESISDERECHO     { $2 }
  ;

ManyBinding
  : Binding ManyBinding                     { $1 : $2 }
  | Binding                                 { [$1] }
  ;

Binding
  : TOKENPARENTESISIZQUIERDO TOKENIDENTIFICADOR Expr TOKENPARENTESISDERECHO
                                           { ($2, $3) }
  ;

-- Cond branches: ( (c1 e1) (c2 e2) ... )
CondBranchList
  : TOKENPARENTESISIZQUIERDO ManyCondBranch TOKENPARENTESISDERECHO  { $2 }
  ;

ManyCondBranch
  : CondBranch ManyCondBranch               { $1 : $2 }
  | CondBranch                              { [$1] }
  ;

CondBranch
  : TOKENPARENTESISIZQUIERDO Expr Expr TOKENPARENTESISDERECHO
                                           { ($2, $3) }
  ;

{
-- Error de parseo (muere con mensaje)
parseError :: [Token] -> a
parseError _ = error "Parse error"
}
