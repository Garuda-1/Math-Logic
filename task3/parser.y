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
  '@'   { TokenForall }
  '?'   { TokenForany }
  '.'   { TokenDot }
  '='   { TokenEq }
  '+'   { TokenAdd }
  '*'   { TokenMul }
  '\''  { TokenAph }
  '0'   { TokenZero }
  pred  { TokenPred $$ }
  var   { TokenVar $$ }

%%

Exp     : Disj                  { $1 } 
        | Disj "->" Exp         { EImpl $1 $3 }

Disj    : Conj                  { $1 }
        | Disj '|' Conj         { EDisj $1 $3 }

Conj    : Unary                 { $1 }
        | Conj '&' Unary        { EConj $1 $3 }

Unary   : Pred                  { $1 }
        | '!' Unary             { ENeg $2 }
        | '(' Exp ')'           { $2 }
        | '@' Var '.' Exp       { EForall $2 $4 }
        | '?' Var '.' Exp       { EForany $2 $4 }

Var     : var                   { EVar $1 }

Pred    : pred                  { EPred $1 }
        | Term '=' Term         { EEq $1 $3 }

Term    : Adden                 { $1 }
        | Term '+' Adden        { EAdd $1 $3 }

Adden   : Multi                 { $1 }
        | Adden '*' Multi       { EMul $1 $3 }

Multi   : Var                   { $1 }
        | '(' Term ')'          { $2 }
        | '0'                   { EZero }
        | Multi '\''            { EInc $1 }

{
parseError :: [Token] -> a
parseError l = error ("Parsing failed " ++ (show l))

data Exp 
  = EImpl Exp Exp
  | EDisj Exp Exp
  | EConj Exp Exp
  | ENeg Exp
  | EForall Exp Exp
  | EForany Exp Exp
  | EVar String
  | EPred String
  | EEq Exp Exp
  | EAdd Exp Exp
  | EMul Exp Exp
  | EZero
  | EInc Exp
  deriving (Eq, Ord)


instance Show Exp where
  show (EImpl x y) = "(" ++ (show x) ++ "->" ++ (show y) ++ ")"
  show (EDisj x y) = "(" ++ (show x) ++ "|" ++ (show y) ++ ")"
  show (EConj x y) = "(" ++ (show x) ++ "&" ++ (show y) ++ ")"
  show (ENeg x) = "(!" ++ (show x) ++ ")"
  show (EForall x y) = "(@" ++ (show x) ++ "." ++ (show y) ++ ")"
  show (EForany x y) = "(?" ++ (show x) ++ "." ++ (show y) ++ ")"
  show (EVar s) = s
  show (EPred s) = s
  show (EEq x y) = "(" ++ (show x) ++ "=" ++ (show y) ++ ")"
  show (EAdd x y) = "(" ++ (show x) ++ "+" ++ (show y) ++ ")"
  show (EMul x y) = "(" ++ (show x) ++ "*" ++ (show y) ++ ")"
  show (EZero) = "0"
  show (EInc x) = (show x) ++ "\'"


{-
instance Show Exp where
  show (EImpl x y) = "(" ++ (show x) ++ " -> " ++ (show y) ++ ")"
  show (EDisj x y) = "(" ++ (show x) ++ " | " ++ (show y) ++ ")"
  show (EConj x y) = "(" ++ (show x) ++ " & " ++ (show y) ++ ")"
  show (ENeg x) = "!" ++ (show x) ++ ""
  show (EForall x y) = "(@" ++ (show x) ++ "." ++ (show y) ++ ")"
  show (EForany x y) = "(?" ++ (show x) ++ "." ++ (show y) ++ ")"
  show (EVar s) = s
  show (EPred s) = s
  show (EEq x y) = "(" ++ (show x) ++ "=" ++ (show y) ++ ")"
  show (EAdd x y) = "(" ++ (show x) ++ "+" ++ (show y) ++ ")"
  show (EMul x y) = "(" ++ (show x) ++ "*" ++ (show y) ++ ")"
  show (EZero) = "0"
  show (EInc x) = (show x) ++ "\'"
-}
}














