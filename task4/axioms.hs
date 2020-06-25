module Axioms where

import Parser



isAx1 :: Exp -> Bool
isAx1 (EImpl a0 (EImpl b1 a1))
  | (a0 == a1) = True
  | otherwise = False
isAx1 _ = False

isAx2 :: Exp -> Bool
isAx2 (EImpl (EImpl a0 b0) (EImpl (EImpl a1 (EImpl b1 c1)) (EImpl a2 c2)))
  | a0 == a1 && a1 == a2 && b0 == b1 && c1 == c2 = True
  | otherwise = False
isAx2 _ = False

isAx3 :: Exp -> Bool
isAx3 (EImpl a0 (EImpl b0 (EConj a1 b1)))
  | a0 == a1 && b0 == b1 = True
  | otherwise = False
isAx3 _ = False

isAx4 :: Exp -> Bool
isAx4 (EImpl (EConj a0 b0) a1)
  | a0 == a1 = True
  | otherwise = False
isAx4 _ = False

isAx5 :: Exp -> Bool
isAx5 (EImpl (EConj a0 b0) b1)
  | b0 == b1 = True
  | otherwise = False
isAx5 _ = False

isAx6 :: Exp -> Bool
isAx6 (EImpl a0 (EDisj a1 b1))
  | a0 == a1 = True
  | otherwise = False
isAx6 _ = False

isAx7 :: Exp -> Bool
isAx7 (EImpl b0 (EDisj a1 b1))
  | b0 == b1 = True
  | otherwise = False
isAx7 _ = False

isAx8 :: Exp -> Bool
isAx8 (EImpl (EImpl a0 c0) (EImpl (EImpl b1 c1) (EImpl (EDisj a2 b2) c2)))
  | a0 == a2 && b1 == b2 && c0 == c1 && c1 == c2 = True
  | otherwise = False
isAx8 _ = False

isAx9 :: Exp -> Bool
isAx9 (EImpl (EImpl a0 b0) (EImpl (EImpl a1 (ENeg b1)) (ENeg a2)))
  | a0 == a1 && b0 == b1 && a1 == a2 = True
  | otherwise = False
isAx9 _ = False 

isAx10 :: Exp -> Bool
isAx10 (EImpl (ENeg (ENeg a0)) a1)
  | a0 == a1 = True
  | otherwise = False
isAx10 _ = False


isAx :: Exp -> Bool
isAx exp =
  if (isAx1 exp) then
    True
  else if (isAx2 exp) then
    True
  else if (isAx3 exp) then
    True
  else if (isAx4 exp) then
    True
  else if (isAx5 exp) then
    True
  else if (isAx6 exp) then
    True
  else if (isAx7 exp) then
    True
  else if (isAx8 exp) then
    True
  else if (isAx9 exp) then
    True
  else if (isAx10 exp) then
    True
  else
    False  

