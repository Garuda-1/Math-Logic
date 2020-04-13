{
module Main where

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

Exp     : Disj                  { EDisj $1 } 
        | Disj "->" Exp         { EImpl $1 $3 }

Disj    : Conj                  { DConj $1 }
        | Disj '|' Conj         { DDisj $1 $3 }

Conj    : Neg                   { CNeg $1 }
        | Conj '&' Neg          { CConj $1 $3 }

Neg     : '!' Neg               { NNeg $2 }
        | var                   { NVar $1 }
        | '(' Exp ')'           { NBrackets $2 }

{
parseError :: [Token] -> a
parseError _ = error "Parsing failed"

data Exp 
  = EDisj Disj 
  | EImpl Disj Exp

instance Show Exp where
  show (EDisj x) = show x
  show (EImpl x y) = "(->," ++ (show x) ++ "," ++ (show y) ++ ")"

data Disj 
  = DConj Conj 
  | DDisj Disj Conj 
  
instance Show Disj where
  show (DConj x) = show x
  show (DDisj x y) = "(|," ++ (show x) ++ "," ++ (show y) ++ ")"

data Conj
  = CNeg Neg
  | CConj Conj Neg 
  
instance Show Conj where
  show (CNeg x) = show x
  show (CConj x y) = "(&," ++ (show x) ++ "," ++ (show y) ++ ")"

data Neg 
  = NNeg Neg 
  | NVar String 
  | NBrackets Exp 
  
instance Show Neg where
  show (NNeg x) = "(!" ++ (show x) ++ ")"
  show (NVar s) = s
  show (NBrackets x) = show x

{--data Token
  = TokenImply
  | TokenDisj
  | TokenConj
  | TokenNegate
  | TokenLBrace 
  | TokenRBrace 
  | TokenVar String
  deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c : cs)
  | (isSpace c || c == '\t' || c == '\n' || c == '\r') = lexer cs
  | (isAlpha c) = TokenVar parsed : (lexer rest) where
      helper :: String -> String -> (String, String)
      helper acc [] = (acc, [])
      helper acc (c : cs) 
        | (isAlpha c || isDigit c || c == '’') = helper (c : acc) cs
        | otherwise = (acc, c : cs)

      (parsed', rest) = helper [] cs
      parsed = c : reverse parsed'

lexer ('-' : '>' : cs) = TokenImply : lexer cs
lexer ('|' : cs) = TokenDisj : lexer cs
lexer ('&' : cs) = TokenConj : lexer cs
lexer ('!' : cs) = TokenNegate : lexer cs
lexer ('(' : cs) = TokenLBrace : lexer cs
lexer (')' : cs) = TokenRBrace : lexer cs--}

main = getLine >>= print . calc . alexScanTokens
}














