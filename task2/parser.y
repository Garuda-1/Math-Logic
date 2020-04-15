{
module Parser where

import Lex
}

%name calc
%tokentype { Token }
%error { parseError }

%token
  "->"  { TokenImply }
  '|'   { TokenDisj }
  '&'   { TokenConj }
  '!'   { TokenNegate }
  '('   { TokenLBrace }
  ')'   { TokenRBrace }
  var   { TokenVar $$ }

%%

Exp     : Disj                  { $1 } 
        | Disj "->" Exp         { EImpl $1 $3 }

Disj    : Conj                  { $1 }
        | Disj '|' Conj         { EDisj $1 $3 }

Conj    : Neg                   { $1 }
        | Conj '&' Neg          { EConj $1 $3 }

Neg     : '!' Neg               { ENeg $2 }
        | var                   { EVar $1 }
        | '(' Exp ')'           { $2 }

{
parseError :: [Token] -> a
parseError _ = error "Parsing failed"

data Exp 
  = EImpl Exp Exp
  | EDisj Exp Exp
  | EConj Exp Exp
  | ENeg Exp
  | EVar String
  deriving (Eq)

instance Show Exp where
  show (EImpl x y) = "(" ++ (show x) ++ " -> " ++ (show y) ++ ")"
  show (EDisj x y) = "(" ++ (show x) ++ " | " ++ (show y) ++ ")"
  show (EConj x y) = "(" ++ (show x) ++ " & " ++ (show y) ++ ")"
  show (ENeg x) = "!" ++ (show x)
  show (EVar s) = s
}














