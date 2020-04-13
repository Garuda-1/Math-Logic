{
module Lex where
}

%wrapper "basic"

$digit = 0-9
$alpha = A-Z
$aph = ’

tokens :-

  $white ;
  \-\>                          { \s -> TokenImply }
  \|                             { \s -> TokenDisj }
  &                             { \s -> TokenConj }
  !                             { \s -> TokenNegate }
  \(                             { \s -> TokenLBrace }
  \)                             { \s -> TokenRBrace }
  $alpha [$alpha $digit $aph]*  { \s -> TokenVar s }

{
data Token
  = TokenImply
  | TokenDisj
  | TokenConj
  | TokenNegate
  | TokenLBrace
  | TokenRBrace
  | TokenVar String
  deriving (Eq, Show)
}
