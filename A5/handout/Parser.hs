{-# OPTIONS_GHC -w #-}
module Parser where

import EDMinML
import Data.Char

-- parser produced by Happy Version 1.18.10

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 (Exp)
	| HappyAbsSyn11 (Typ)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

action_0 (13) = happyShift action_10
action_0 (14) = happyShift action_11
action_0 (15) = happyShift action_12
action_0 (18) = happyShift action_13
action_0 (20) = happyShift action_14
action_0 (24) = happyShift action_2
action_0 (27) = happyShift action_15
action_0 (29) = happyShift action_16
action_0 (4) = happyGoto action_3
action_0 (5) = happyGoto action_4
action_0 (6) = happyGoto action_5
action_0 (7) = happyGoto action_6
action_0 (8) = happyGoto action_7
action_0 (9) = happyGoto action_8
action_0 (10) = happyGoto action_9
action_0 _ = happyFail

action_1 (24) = happyShift action_2
action_1 _ = happyFail

action_2 (13) = happyShift action_10
action_2 (14) = happyShift action_11
action_2 (15) = happyShift action_12
action_2 (18) = happyShift action_13
action_2 (20) = happyShift action_14
action_2 (24) = happyShift action_2
action_2 (27) = happyShift action_15
action_2 (29) = happyShift action_16
action_2 (4) = happyGoto action_26
action_2 (5) = happyGoto action_4
action_2 (6) = happyGoto action_5
action_2 (7) = happyGoto action_6
action_2 (8) = happyGoto action_7
action_2 (9) = happyGoto action_8
action_2 (10) = happyGoto action_9
action_2 _ = happyFail

action_3 (33) = happyAccept
action_3 _ = happyFail

action_4 _ = happyReduce_5

action_5 (16) = happyShift action_25
action_5 _ = happyReduce_7

action_6 (17) = happyShift action_23
action_6 (18) = happyShift action_24
action_6 _ = happyReduce_10

action_7 (19) = happyShift action_22
action_7 _ = happyReduce_12

action_8 _ = happyReduce_14

action_9 (20) = happyShift action_21
action_9 _ = happyReduce_16

action_10 _ = happyReduce_17

action_11 _ = happyReduce_18

action_12 _ = happyReduce_19

action_13 (13) = happyShift action_10
action_13 (14) = happyShift action_11
action_13 (15) = happyShift action_12
action_13 (20) = happyShift action_14
action_13 (9) = happyGoto action_20
action_13 (10) = happyGoto action_9
action_13 _ = happyFail

action_14 (13) = happyShift action_10
action_14 (14) = happyShift action_11
action_14 (15) = happyShift action_12
action_14 (18) = happyShift action_13
action_14 (20) = happyShift action_14
action_14 (24) = happyShift action_2
action_14 (27) = happyShift action_15
action_14 (29) = happyShift action_16
action_14 (4) = happyGoto action_19
action_14 (5) = happyGoto action_4
action_14 (6) = happyGoto action_5
action_14 (7) = happyGoto action_6
action_14 (8) = happyGoto action_7
action_14 (9) = happyGoto action_8
action_14 (10) = happyGoto action_9
action_14 _ = happyFail

action_15 (15) = happyShift action_18
action_15 _ = happyFail

action_16 (20) = happyShift action_17
action_16 _ = happyFail

action_17 (13) = happyShift action_10
action_17 (14) = happyShift action_11
action_17 (15) = happyShift action_12
action_17 (18) = happyShift action_13
action_17 (20) = happyShift action_14
action_17 (24) = happyShift action_2
action_17 (27) = happyShift action_15
action_17 (29) = happyShift action_16
action_17 (4) = happyGoto action_36
action_17 (5) = happyGoto action_4
action_17 (6) = happyGoto action_5
action_17 (7) = happyGoto action_6
action_17 (8) = happyGoto action_7
action_17 (9) = happyGoto action_8
action_17 (10) = happyGoto action_9
action_17 _ = happyFail

action_18 (15) = happyShift action_34
action_18 (20) = happyShift action_35
action_18 _ = happyFail

action_19 (21) = happyShift action_33
action_19 _ = happyFail

action_20 _ = happyReduce_13

action_21 (13) = happyShift action_10
action_21 (14) = happyShift action_11
action_21 (15) = happyShift action_12
action_21 (18) = happyShift action_13
action_21 (20) = happyShift action_14
action_21 (24) = happyShift action_2
action_21 (27) = happyShift action_15
action_21 (29) = happyShift action_16
action_21 (4) = happyGoto action_32
action_21 (5) = happyGoto action_4
action_21 (6) = happyGoto action_5
action_21 (7) = happyGoto action_6
action_21 (8) = happyGoto action_7
action_21 (9) = happyGoto action_8
action_21 (10) = happyGoto action_9
action_21 _ = happyFail

action_22 (13) = happyShift action_10
action_22 (14) = happyShift action_11
action_22 (15) = happyShift action_12
action_22 (18) = happyShift action_13
action_22 (20) = happyShift action_14
action_22 (7) = happyGoto action_31
action_22 (8) = happyGoto action_7
action_22 (9) = happyGoto action_8
action_22 (10) = happyGoto action_9
action_22 _ = happyFail

action_23 (13) = happyShift action_10
action_23 (14) = happyShift action_11
action_23 (15) = happyShift action_12
action_23 (18) = happyShift action_13
action_23 (20) = happyShift action_14
action_23 (6) = happyGoto action_30
action_23 (7) = happyGoto action_6
action_23 (8) = happyGoto action_7
action_23 (9) = happyGoto action_8
action_23 (10) = happyGoto action_9
action_23 _ = happyFail

action_24 (13) = happyShift action_10
action_24 (14) = happyShift action_11
action_24 (15) = happyShift action_12
action_24 (18) = happyShift action_13
action_24 (20) = happyShift action_14
action_24 (6) = happyGoto action_29
action_24 (7) = happyGoto action_6
action_24 (8) = happyGoto action_7
action_24 (9) = happyGoto action_8
action_24 (10) = happyGoto action_9
action_24 _ = happyFail

action_25 (13) = happyShift action_10
action_25 (14) = happyShift action_11
action_25 (15) = happyShift action_12
action_25 (18) = happyShift action_13
action_25 (20) = happyShift action_14
action_25 (5) = happyGoto action_28
action_25 (6) = happyGoto action_5
action_25 (7) = happyGoto action_6
action_25 (8) = happyGoto action_7
action_25 (9) = happyGoto action_8
action_25 (10) = happyGoto action_9
action_25 _ = happyFail

action_26 (25) = happyShift action_27
action_26 _ = happyFail

action_27 (13) = happyShift action_10
action_27 (14) = happyShift action_11
action_27 (15) = happyShift action_12
action_27 (18) = happyShift action_13
action_27 (20) = happyShift action_14
action_27 (24) = happyShift action_2
action_27 (27) = happyShift action_15
action_27 (29) = happyShift action_16
action_27 (4) = happyGoto action_41
action_27 (5) = happyGoto action_4
action_27 (6) = happyGoto action_5
action_27 (7) = happyGoto action_6
action_27 (8) = happyGoto action_7
action_27 (9) = happyGoto action_8
action_27 (10) = happyGoto action_9
action_27 _ = happyFail

action_28 _ = happyReduce_6

action_29 _ = happyReduce_9

action_30 _ = happyReduce_8

action_31 _ = happyReduce_11

action_32 (21) = happyShift action_40
action_32 _ = happyFail

action_33 _ = happyReduce_20

action_34 (16) = happyShift action_39
action_34 _ = happyFail

action_35 (15) = happyShift action_38
action_35 _ = happyFail

action_36 (22) = happyShift action_37
action_36 _ = happyFail

action_37 (20) = happyShift action_47
action_37 (30) = happyShift action_48
action_37 (31) = happyShift action_49
action_37 (11) = happyGoto action_45
action_37 (12) = happyGoto action_46
action_37 _ = happyFail

action_38 (23) = happyShift action_44
action_38 _ = happyFail

action_39 (13) = happyShift action_10
action_39 (14) = happyShift action_11
action_39 (15) = happyShift action_12
action_39 (18) = happyShift action_13
action_39 (20) = happyShift action_14
action_39 (24) = happyShift action_2
action_39 (27) = happyShift action_15
action_39 (29) = happyShift action_16
action_39 (4) = happyGoto action_43
action_39 (5) = happyGoto action_4
action_39 (6) = happyGoto action_5
action_39 (7) = happyGoto action_6
action_39 (8) = happyGoto action_7
action_39 (9) = happyGoto action_8
action_39 (10) = happyGoto action_9
action_39 _ = happyFail

action_40 _ = happyReduce_15

action_41 (26) = happyShift action_42
action_41 _ = happyFail

action_42 (13) = happyShift action_10
action_42 (14) = happyShift action_11
action_42 (15) = happyShift action_12
action_42 (18) = happyShift action_13
action_42 (20) = happyShift action_14
action_42 (24) = happyShift action_2
action_42 (27) = happyShift action_15
action_42 (29) = happyShift action_16
action_42 (4) = happyGoto action_55
action_42 (5) = happyGoto action_4
action_42 (6) = happyGoto action_5
action_42 (7) = happyGoto action_6
action_42 (8) = happyGoto action_7
action_42 (9) = happyGoto action_8
action_42 (10) = happyGoto action_9
action_42 _ = happyFail

action_43 (28) = happyShift action_54
action_43 _ = happyFail

action_44 (20) = happyShift action_47
action_44 (30) = happyShift action_48
action_44 (31) = happyShift action_49
action_44 (11) = happyGoto action_53
action_44 (12) = happyGoto action_46
action_44 _ = happyFail

action_45 (21) = happyShift action_52
action_45 _ = happyFail

action_46 (32) = happyShift action_51
action_46 _ = happyReduce_22

action_47 (20) = happyShift action_47
action_47 (30) = happyShift action_48
action_47 (31) = happyShift action_49
action_47 (11) = happyGoto action_50
action_47 (12) = happyGoto action_46
action_47 _ = happyFail

action_48 _ = happyReduce_23

action_49 _ = happyReduce_24

action_50 (21) = happyShift action_58
action_50 _ = happyFail

action_51 (20) = happyShift action_47
action_51 (30) = happyShift action_48
action_51 (31) = happyShift action_49
action_51 (11) = happyGoto action_57
action_51 (12) = happyGoto action_46
action_51 _ = happyFail

action_52 _ = happyReduce_4

action_53 (21) = happyShift action_56
action_53 _ = happyFail

action_54 _ = happyReduce_3

action_55 _ = happyReduce_1

action_56 (23) = happyShift action_59
action_56 _ = happyFail

action_57 _ = happyReduce_21

action_58 _ = happyReduce_25

action_59 (20) = happyShift action_47
action_59 (30) = happyShift action_48
action_59 (31) = happyShift action_49
action_59 (11) = happyGoto action_60
action_59 (12) = happyGoto action_46
action_59 _ = happyFail

action_60 (16) = happyShift action_61
action_60 _ = happyFail

action_61 (13) = happyShift action_10
action_61 (14) = happyShift action_11
action_61 (15) = happyShift action_12
action_61 (18) = happyShift action_13
action_61 (20) = happyShift action_14
action_61 (24) = happyShift action_2
action_61 (27) = happyShift action_15
action_61 (29) = happyShift action_16
action_61 (4) = happyGoto action_62
action_61 (5) = happyGoto action_4
action_61 (6) = happyGoto action_5
action_61 (7) = happyGoto action_6
action_61 (8) = happyGoto action_7
action_61 (9) = happyGoto action_8
action_61 (10) = happyGoto action_9
action_61 _ = happyFail

action_62 (28) = happyShift action_63
action_62 _ = happyFail

action_63 _ = happyReduce_2

happyReduce_1 = happyReduce 6 4 happyReduction_1
happyReduction_1 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (If happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_2 = happyReduce 12 4 happyReduction_2
happyReduction_2 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_11) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_9) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenId happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenId happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Fun happy_var_2 happy_var_4 happy_var_6 happy_var_9 happy_var_11
	) `HappyStk` happyRest

happyReduce_3 = happyReduce 6 4 happyReduction_3
happyReduction_3 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenId happy_var_3)) `HappyStk`
	(HappyTerminal (TokenId happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (UTFun happy_var_2 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_4 = happyReduce 6 4 happyReduction_4
happyReduction_4 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Check happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_5 = happySpecReduce_1  4 happyReduction_5
happyReduction_5 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  5 happyReduction_6
happyReduction_6 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (PrimOp Equal [happy_var_1,happy_var_3]
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  5 happyReduction_7
happyReduction_7 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  6 happyReduction_8
happyReduction_8 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (PrimOp Plus [happy_var_1,happy_var_3]
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  6 happyReduction_9
happyReduction_9 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (PrimOp Minus [happy_var_1,happy_var_3]
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  6 happyReduction_10
happyReduction_10 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  7 happyReduction_11
happyReduction_11 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (PrimOp Times [happy_var_1,happy_var_3]
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  7 happyReduction_12
happyReduction_12 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_2  8 happyReduction_13
happyReduction_13 (HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (PrimOp Negate [happy_var_2]
	)
happyReduction_13 _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  8 happyReduction_14
happyReduction_14 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happyReduce 4 9 happyReduction_15
happyReduction_15 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Apply happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_16 = happySpecReduce_1  9 happyReduction_16
happyReduction_16 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  10 happyReduction_17
happyReduction_17 (HappyTerminal (TokenInt happy_var_1))
	 =  HappyAbsSyn4
		 (Int happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  10 happyReduction_18
happyReduction_18 (HappyTerminal (TokenBool happy_var_1))
	 =  HappyAbsSyn4
		 (Bool happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  10 happyReduction_19
happyReduction_19 (HappyTerminal (TokenId happy_var_1))
	 =  HappyAbsSyn4
		 (Var happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  10 happyReduction_20
happyReduction_20 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  11 happyReduction_21
happyReduction_21 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (ARROW happy_var_1 happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  11 happyReduction_22
happyReduction_22 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  12 happyReduction_23
happyReduction_23 _
	 =  HappyAbsSyn11
		 (INT
	)

happyReduce_24 = happySpecReduce_1  12 happyReduction_24
happyReduction_24 _
	 =  HappyAbsSyn11
		 (BOOL
	)

happyReduce_25 = happySpecReduce_3  12 happyReduction_25
happyReduction_25 _
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (happy_var_2
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 33 33 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenInt happy_dollar_dollar -> cont 13;
	TokenBool happy_dollar_dollar -> cont 14;
	TokenId happy_dollar_dollar -> cont 15;
	TokenEq -> cont 16;
	TokenPlus -> cont 17;
	TokenMinus -> cont 18;
	TokenTimes -> cont 19;
	TokenOB -> cont 20;
	TokenCB -> cont 21;
	TokenComma -> cont 22;
	TokenColon -> cont 23;
	TokenIf -> cont 24;
	TokenThen -> cont 25;
	TokenElse -> cont 26;
	TokenFun -> cont 27;
	TokenEnd -> cont 28;
	TokenCheck -> cont 29;
	TokenIntTy -> cont 30;
	TokenBoolTy -> cont 31;
	TokenArrow -> cont 32;
	_ -> happyError' (tk:tks)
	}

happyError_ 33 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . happyError

parser tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


happyError :: [Token] -> a
happyError _ = error ("Parse error\n")

data Token =
    TokenInt Int
  | TokenBool Bool
  | TokenId String
  | TokenEq
  | TokenPlus
  | TokenMinus
  | TokenTimes
  | TokenOB
  | TokenCB
  | TokenComma
  | TokenColon
  | TokenIf
  | TokenThen
  | TokenElse
  | TokenFun
  | TokenEnd
  | TokenCheck
  | TokenIntTy
  | TokenBoolTy
  | TokenArrow

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) 
  | isSpace c = lexer cs
  | isAlpha c = lexId (c:cs)
  | isDigit c = lexNum (c:cs)
lexer ('-':'>':cs) = TokenArrow : lexer cs
lexer ('=':cs) = TokenEq : lexer cs
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('-':cs) = TokenMinus : lexer cs
lexer ('*':cs) = TokenTimes : lexer cs
lexer ('(':cs) = TokenOB : lexer cs
lexer (')':cs) = TokenCB : lexer cs
lexer (',':cs) = TokenComma : lexer cs
lexer (':':cs) = TokenColon : lexer cs

lexNum :: String -> [Token]
lexNum cs = TokenInt (read num) : lexer rest
	where (num,rest) = span isDigit cs

lexId :: String -> [Token]
lexId cs =
  case span isAlpha cs of
    ("if",rest) -> TokenIf : lexer rest
    ("then",rest)  -> TokenThen : lexer rest
    ("else",rest)  -> TokenElse : lexer rest
    ("fun",rest)  -> TokenFun : lexer rest
    ("end",rest)  -> TokenEnd : lexer rest
    ("check",rest)  -> TokenCheck : lexer rest
    ("int",rest)  -> TokenIntTy : lexer rest
    ("bool",rest)  -> TokenBoolTy : lexer rest
    ("true",rest) -> TokenBool True : lexer rest
    ("false",rest) -> TokenBool False : lexer rest
    (id,rest)   -> TokenId id : lexer rest
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates/GenericTemplate.hs" #-}








{-# LINE 51 "templates/GenericTemplate.hs" #-}

{-# LINE 61 "templates/GenericTemplate.hs" #-}

{-# LINE 70 "templates/GenericTemplate.hs" #-}

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

{-# LINE 148 "templates/GenericTemplate.hs" #-}

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
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 246 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--	trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 312 "templates/GenericTemplate.hs" #-}
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
