{-# OPTIONS_GHC -w #-}
module Parser where

import Lex
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.8

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,131) ([32768,3597,45056,449,128,0,0,0,0,0,0,0,0,0,3,32768,0,8192,0,14390,49152,1798,0,128,0,16,0,0,0,0,0,0,0,0,0,1728,7,57560,0,32,0,4,4096,0,25088,0,0,0,0,32768,1280,4096,160,512,20,33632,3,0,0,128,8192,320,0,1,32768,0,0,0,0,24576,899,27648,112,0,0,0,0,0,0,0,8192,4,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_calc","Exp","Disj","Conj","Unary","Var","Pred","Term","Adden","Multi","\"->\"","'|'","'&'","'!'","'('","')'","'@'","'?'","'.'","'='","'+'","'*'","'\\''","'0'","pred","var","%eof"]
        bit_start = st * 29
        bit_end = (st + 1) * 29
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..28]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (16) = happyShift action_10
action_0 (17) = happyShift action_11
action_0 (19) = happyShift action_12
action_0 (20) = happyShift action_13
action_0 (26) = happyShift action_14
action_0 (27) = happyShift action_15
action_0 (28) = happyShift action_16
action_0 (4) = happyGoto action_17
action_0 (5) = happyGoto action_18
action_0 (6) = happyGoto action_3
action_0 (7) = happyGoto action_4
action_0 (8) = happyGoto action_5
action_0 (9) = happyGoto action_6
action_0 (10) = happyGoto action_7
action_0 (11) = happyGoto action_8
action_0 (12) = happyGoto action_9
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (16) = happyShift action_10
action_1 (17) = happyShift action_11
action_1 (19) = happyShift action_12
action_1 (20) = happyShift action_13
action_1 (26) = happyShift action_14
action_1 (27) = happyShift action_15
action_1 (28) = happyShift action_16
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 (7) = happyGoto action_4
action_1 (8) = happyGoto action_5
action_1 (9) = happyGoto action_6
action_1 (10) = happyGoto action_7
action_1 (11) = happyGoto action_8
action_1 (12) = happyGoto action_9
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (14) = happyShift action_20
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (15) = happyShift action_30
action_3 _ = happyReduce_3

action_4 _ = happyReduce_5

action_5 _ = happyReduce_19

action_6 _ = happyReduce_7

action_7 (22) = happyShift action_28
action_7 (23) = happyShift action_29
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (24) = happyShift action_27
action_8 _ = happyReduce_15

action_9 (25) = happyShift action_26
action_9 _ = happyReduce_17

action_10 (16) = happyShift action_10
action_10 (17) = happyShift action_11
action_10 (19) = happyShift action_12
action_10 (20) = happyShift action_13
action_10 (26) = happyShift action_14
action_10 (27) = happyShift action_15
action_10 (28) = happyShift action_16
action_10 (7) = happyGoto action_25
action_10 (8) = happyGoto action_5
action_10 (9) = happyGoto action_6
action_10 (10) = happyGoto action_7
action_10 (11) = happyGoto action_8
action_10 (12) = happyGoto action_9
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (16) = happyShift action_10
action_11 (17) = happyShift action_11
action_11 (19) = happyShift action_12
action_11 (20) = happyShift action_13
action_11 (26) = happyShift action_14
action_11 (27) = happyShift action_15
action_11 (28) = happyShift action_16
action_11 (4) = happyGoto action_23
action_11 (5) = happyGoto action_18
action_11 (6) = happyGoto action_3
action_11 (7) = happyGoto action_4
action_11 (8) = happyGoto action_5
action_11 (9) = happyGoto action_6
action_11 (10) = happyGoto action_24
action_11 (11) = happyGoto action_8
action_11 (12) = happyGoto action_9
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (28) = happyShift action_16
action_12 (8) = happyGoto action_22
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (28) = happyShift action_16
action_13 (8) = happyGoto action_21
action_13 _ = happyFail (happyExpListPerState 13)

action_14 _ = happyReduce_21

action_15 _ = happyReduce_13

action_16 _ = happyReduce_12

action_17 (29) = happyAccept
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (13) = happyShift action_19
action_18 (14) = happyShift action_20
action_18 _ = happyReduce_1

action_19 (16) = happyShift action_10
action_19 (17) = happyShift action_11
action_19 (19) = happyShift action_12
action_19 (20) = happyShift action_13
action_19 (26) = happyShift action_14
action_19 (27) = happyShift action_15
action_19 (28) = happyShift action_16
action_19 (4) = happyGoto action_41
action_19 (5) = happyGoto action_18
action_19 (6) = happyGoto action_3
action_19 (7) = happyGoto action_4
action_19 (8) = happyGoto action_5
action_19 (9) = happyGoto action_6
action_19 (10) = happyGoto action_7
action_19 (11) = happyGoto action_8
action_19 (12) = happyGoto action_9
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (16) = happyShift action_10
action_20 (17) = happyShift action_11
action_20 (19) = happyShift action_12
action_20 (20) = happyShift action_13
action_20 (26) = happyShift action_14
action_20 (27) = happyShift action_15
action_20 (28) = happyShift action_16
action_20 (6) = happyGoto action_40
action_20 (7) = happyGoto action_4
action_20 (8) = happyGoto action_5
action_20 (9) = happyGoto action_6
action_20 (10) = happyGoto action_7
action_20 (11) = happyGoto action_8
action_20 (12) = happyGoto action_9
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (21) = happyShift action_39
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (21) = happyShift action_38
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (18) = happyShift action_37
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (18) = happyShift action_36
action_24 (22) = happyShift action_28
action_24 (23) = happyShift action_29
action_24 _ = happyFail (happyExpListPerState 24)

action_25 _ = happyReduce_8

action_26 _ = happyReduce_22

action_27 (17) = happyShift action_33
action_27 (26) = happyShift action_14
action_27 (28) = happyShift action_16
action_27 (8) = happyGoto action_5
action_27 (12) = happyGoto action_35
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (17) = happyShift action_33
action_28 (26) = happyShift action_14
action_28 (28) = happyShift action_16
action_28 (8) = happyGoto action_5
action_28 (10) = happyGoto action_34
action_28 (11) = happyGoto action_8
action_28 (12) = happyGoto action_9
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (17) = happyShift action_33
action_29 (26) = happyShift action_14
action_29 (28) = happyShift action_16
action_29 (8) = happyGoto action_5
action_29 (11) = happyGoto action_32
action_29 (12) = happyGoto action_9
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (16) = happyShift action_10
action_30 (17) = happyShift action_11
action_30 (19) = happyShift action_12
action_30 (20) = happyShift action_13
action_30 (26) = happyShift action_14
action_30 (27) = happyShift action_15
action_30 (28) = happyShift action_16
action_30 (7) = happyGoto action_31
action_30 (8) = happyGoto action_5
action_30 (9) = happyGoto action_6
action_30 (10) = happyGoto action_7
action_30 (11) = happyGoto action_8
action_30 (12) = happyGoto action_9
action_30 _ = happyFail (happyExpListPerState 30)

action_31 _ = happyReduce_6

action_32 (24) = happyShift action_27
action_32 _ = happyReduce_16

action_33 (17) = happyShift action_33
action_33 (26) = happyShift action_14
action_33 (28) = happyShift action_16
action_33 (8) = happyGoto action_5
action_33 (10) = happyGoto action_44
action_33 (11) = happyGoto action_8
action_33 (12) = happyGoto action_9
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (23) = happyShift action_29
action_34 _ = happyReduce_14

action_35 (25) = happyShift action_26
action_35 _ = happyReduce_18

action_36 _ = happyReduce_20

action_37 _ = happyReduce_9

action_38 (16) = happyShift action_10
action_38 (17) = happyShift action_11
action_38 (19) = happyShift action_12
action_38 (20) = happyShift action_13
action_38 (26) = happyShift action_14
action_38 (27) = happyShift action_15
action_38 (28) = happyShift action_16
action_38 (4) = happyGoto action_43
action_38 (5) = happyGoto action_18
action_38 (6) = happyGoto action_3
action_38 (7) = happyGoto action_4
action_38 (8) = happyGoto action_5
action_38 (9) = happyGoto action_6
action_38 (10) = happyGoto action_7
action_38 (11) = happyGoto action_8
action_38 (12) = happyGoto action_9
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (16) = happyShift action_10
action_39 (17) = happyShift action_11
action_39 (19) = happyShift action_12
action_39 (20) = happyShift action_13
action_39 (26) = happyShift action_14
action_39 (27) = happyShift action_15
action_39 (28) = happyShift action_16
action_39 (4) = happyGoto action_42
action_39 (5) = happyGoto action_18
action_39 (6) = happyGoto action_3
action_39 (7) = happyGoto action_4
action_39 (8) = happyGoto action_5
action_39 (9) = happyGoto action_6
action_39 (10) = happyGoto action_7
action_39 (11) = happyGoto action_8
action_39 (12) = happyGoto action_9
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (15) = happyShift action_30
action_40 _ = happyReduce_4

action_41 _ = happyReduce_2

action_42 _ = happyReduce_11

action_43 _ = happyReduce_10

action_44 (18) = happyShift action_36
action_44 (23) = happyShift action_29
action_44 _ = happyFail (happyExpListPerState 44)

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_3  4 happyReduction_2
happyReduction_2 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (EImpl happy_var_1 happy_var_3
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  5 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  5 happyReduction_4
happyReduction_4 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (EDisj happy_var_1 happy_var_3
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  6 happyReduction_5
happyReduction_5 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  6 happyReduction_6
happyReduction_6 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (EConj happy_var_1 happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  7 happyReduction_7
happyReduction_7 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_2  7 happyReduction_8
happyReduction_8 (HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (ENeg happy_var_2
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  7 happyReduction_9
happyReduction_9 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happyReduce 4 7 happyReduction_10
happyReduction_10 ((HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (EForall happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_11 = happyReduce 4 7 happyReduction_11
happyReduction_11 ((HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (EForany happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_12 = happySpecReduce_1  8 happyReduction_12
happyReduction_12 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn8
		 (EVar happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  9 happyReduction_13
happyReduction_13 (HappyTerminal (TokenPred happy_var_1))
	 =  HappyAbsSyn9
		 (EPred happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  9 happyReduction_14
happyReduction_14 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (EEq happy_var_1 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  10 happyReduction_15
happyReduction_15 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  10 happyReduction_16
happyReduction_16 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (EAdd happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  11 happyReduction_17
happyReduction_17 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  11 happyReduction_18
happyReduction_18 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (EMul happy_var_1 happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  12 happyReduction_19
happyReduction_19 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  12 happyReduction_20
happyReduction_20 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (happy_var_2
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  12 happyReduction_21
happyReduction_21 _
	 =  HappyAbsSyn12
		 (EZero
	)

happyReduce_22 = happySpecReduce_2  12 happyReduction_22
happyReduction_22 _
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (EInc happy_var_1
	)
happyReduction_22 _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 29 29 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenImply -> cont 13;
	TokenDisj -> cont 14;
	TokenConj -> cont 15;
	TokenNegate -> cont 16;
	TokenLBrace -> cont 17;
	TokenRBrace -> cont 18;
	TokenForall -> cont 19;
	TokenForany -> cont 20;
	TokenDot -> cont 21;
	TokenEq -> cont 22;
	TokenAdd -> cont 23;
	TokenMul -> cont 24;
	TokenAph -> cont 25;
	TokenZero -> cont 26;
	TokenPred happy_dollar_dollar -> cont 27;
	TokenVar happy_dollar_dollar -> cont 28;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 29 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [String]) -> HappyIdentity a
happyError' = HappyIdentity . (\(tokens, _) -> parseError tokens)
calc tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


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
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}

















{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc8814_0/ghc_2.h" #-}




























































































































































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 









{-# LINE 43 "templates/GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Int Happy_IntList







{-# LINE 65 "templates/GenericTemplate.hs" #-}

{-# LINE 75 "templates/GenericTemplate.hs" #-}

{-# LINE 84 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 137 "templates/GenericTemplate.hs" #-}

{-# LINE 147 "templates/GenericTemplate.hs" #-}
indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 267 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 333 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
