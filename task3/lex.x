{
module Lex where
}

%wrapper "basic"

$alphaPred = A-Z
$alphaVar = a-z

tokens :-

  $white ;
  \-\>                          { \s -> TokenImply }
  \|                            { \s -> TokenDisj }
  &                             { \s -> TokenConj }
  !                             { \s -> TokenNegate }
  \(                            { \s -> TokenLBrace }
  \)                            { \s -> TokenRBrace }
  @                             { \s -> TokenForall }
  \?                            { \s -> TokenForany }
  \.                            { \s -> TokenDot }
  =                             { \s -> TokenEq }
  \+                            { \s -> TokenAdd }
  \*                            { \s -> TokenMul }
  \'                            { \s -> TokenAph }
  0                             { \s -> TokenZero }
  $alphaPred                    { \s -> TokenPred s}
  $alphaVar                     { \s -> TokenVar s }

{
data Token
  = TokenImply
  | TokenDisj
  | TokenConj
  | TokenNegate
  | TokenLBrace
  | TokenRBrace
  | TokenForall
  | TokenForany
  | TokenDot
  | TokenEq
  | TokenAdd
  | TokenMul
  | TokenAph
  | TokenZero
  | TokenPred String
  | TokenVar String
  deriving (Eq, Show)
}
