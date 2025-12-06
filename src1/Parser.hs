{-# OPTIONS_GHC -w #-}
module Parser where
import SASA
import Lexer    -- token type comes from aquí

-- El parser generado devolverá un SASA
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,199) ([32768,2,32768,3,20,0,28,0,0,0,58624,65527,2047,26624,0,14336,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,1,0,0,0,0,0,40,0,56,64,0,0,512,0,0,4096,0,0,32768,2,32768,3,20,0,28,32,0,0,256,0,0,10240,0,14336,16384,1,49152,1,10,0,14,80,0,112,640,0,896,5120,0,7168,40960,0,57344,0,5,0,7,40,0,56,320,0,448,2560,0,3584,20480,0,28672,32768,2,32768,3,20,0,28,160,0,224,1280,0,1792,10240,0,14336,16384,1,49152,1,10,0,14,80,0,112,640,0,896,2048,0,0,16384,0,0,0,2,0,0,16,0,0,128,0,0,1024,0,0,8192,0,0,0,1,0,0,8,0,0,64,0,0,1280,0,1792,4096,0,0,32768,0,0,0,4,0,0,32,0,0,256,0,0,2048,0,0,16384,0,0,0,0,0,4,16400,0,0,64,0,0,2560,0,3584,20480,0,28672,32768,2,32768,3,4,0,0,160,0,224,1280,0,1792,4096,0,0,0,0,0,0,0,0,0,80,0,112,0,0,0,0,0,0,16384,0,0,0,2,0,0,16,0,0,64,0,0,0,0,2048,8192,0,0,32768,2,32768,3,20,0,28,64,0,0,256,0,0,10240,0,14336,0,0,0,0,10,0,14,32,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,32768,2,32768,3,8,0,0,160,0,224,0,0,0,0,0,0,32768,0,0,0,4,0,0,0,0,0,640,0,896,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","SASA","Expr","ExprList","ParamList","ListElems","ListElemsRest","BindingList","ManyBinding","Binding","CondBranchList","ManyCondBranch","CondBranch","TOKENPARENTESISIZQUIERDO","TOKENPARENTESISDERECHO","TOKENCORCHETEIZQUIERDO","TOKENCORCHETEDERECHO","TOKENCOMA","TOKENLET","TOKENLETREC","TOKENLETESTRELLA","TOKENIF","TOKENIFCERO","TOKENCOND","TOKENELSE","TOKENLAMBDA","TOKENPRIMERO","TOKENSEGUNDO","TOKENCABEZA","TOKENCOLA","TOKENMASUNO","TOKENMENOSUNO","TOKENRAIZCUADRADA","TOKENPOTENCIA","TOKENSUMA","TOKENRESTA","TOKENMULTIPLICACION","TOKENDIVISION","TOKENIGUAL","TOKENMENORQUE","TOKENMAYORQUE","TOKENMENORIGUALQUE","TOKENMAYORIGUALQUE","TOKENDIFERENTEQUE","TOKENPAIR","TOKENBOOLEANO","TOKENENTERO","TOKENIDENTIFICADOR","%eof"]
        bit_start = st Prelude.* 51
        bit_end = (st Prelude.+ 1) Prelude.* 51
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..50]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (16) = happyShift action_3
action_0 (18) = happyShift action_4
action_0 (48) = happyShift action_5
action_0 (49) = happyShift action_6
action_0 (50) = happyShift action_7
action_0 (4) = happyGoto action_8
action_0 (5) = happyGoto action_2
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (16) = happyShift action_3
action_1 (18) = happyShift action_4
action_1 (48) = happyShift action_5
action_1 (49) = happyShift action_6
action_1 (50) = happyShift action_7
action_1 (5) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (16) = happyShift action_3
action_3 (18) = happyShift action_4
action_3 (21) = happyShift action_13
action_3 (22) = happyShift action_14
action_3 (23) = happyShift action_15
action_3 (24) = happyShift action_16
action_3 (25) = happyShift action_17
action_3 (26) = happyShift action_18
action_3 (28) = happyShift action_19
action_3 (29) = happyShift action_20
action_3 (30) = happyShift action_21
action_3 (31) = happyShift action_22
action_3 (32) = happyShift action_23
action_3 (33) = happyShift action_24
action_3 (34) = happyShift action_25
action_3 (35) = happyShift action_26
action_3 (36) = happyShift action_27
action_3 (37) = happyShift action_28
action_3 (38) = happyShift action_29
action_3 (39) = happyShift action_30
action_3 (40) = happyShift action_31
action_3 (41) = happyShift action_32
action_3 (42) = happyShift action_33
action_3 (43) = happyShift action_34
action_3 (44) = happyShift action_35
action_3 (45) = happyShift action_36
action_3 (46) = happyShift action_37
action_3 (47) = happyShift action_38
action_3 (48) = happyShift action_5
action_3 (49) = happyShift action_6
action_3 (50) = happyShift action_7
action_3 (5) = happyGoto action_12
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (16) = happyShift action_3
action_4 (18) = happyShift action_4
action_4 (19) = happyShift action_11
action_4 (48) = happyShift action_5
action_4 (49) = happyShift action_6
action_4 (50) = happyShift action_7
action_4 (5) = happyGoto action_9
action_4 (8) = happyGoto action_10
action_4 _ = happyFail (happyExpListPerState 4)

action_5 _ = happyReduce_3

action_6 _ = happyReduce_2

action_7 _ = happyReduce_4

action_8 (51) = happyAccept
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (20) = happyShift action_71
action_9 (9) = happyGoto action_70
action_9 _ = happyReduce_41

action_10 (19) = happyShift action_69
action_10 _ = happyFail (happyExpListPerState 10)

action_11 _ = happyReduce_24

action_12 (16) = happyShift action_3
action_12 (18) = happyShift action_4
action_12 (48) = happyShift action_5
action_12 (49) = happyShift action_6
action_12 (50) = happyShift action_7
action_12 (5) = happyGoto action_40
action_12 (6) = happyGoto action_68
action_12 _ = happyReduce_35

action_13 (16) = happyShift action_65
action_13 (10) = happyGoto action_67
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (16) = happyShift action_65
action_14 (10) = happyGoto action_66
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (16) = happyShift action_65
action_15 (10) = happyGoto action_64
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (16) = happyShift action_3
action_16 (18) = happyShift action_4
action_16 (48) = happyShift action_5
action_16 (49) = happyShift action_6
action_16 (50) = happyShift action_7
action_16 (5) = happyGoto action_63
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (16) = happyShift action_3
action_17 (18) = happyShift action_4
action_17 (48) = happyShift action_5
action_17 (49) = happyShift action_6
action_17 (50) = happyShift action_7
action_17 (5) = happyGoto action_62
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (16) = happyShift action_61
action_18 (13) = happyGoto action_60
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (16) = happyShift action_59
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (16) = happyShift action_3
action_20 (18) = happyShift action_4
action_20 (48) = happyShift action_5
action_20 (49) = happyShift action_6
action_20 (50) = happyShift action_7
action_20 (5) = happyGoto action_58
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (16) = happyShift action_3
action_21 (18) = happyShift action_4
action_21 (48) = happyShift action_5
action_21 (49) = happyShift action_6
action_21 (50) = happyShift action_7
action_21 (5) = happyGoto action_57
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (16) = happyShift action_3
action_22 (18) = happyShift action_4
action_22 (48) = happyShift action_5
action_22 (49) = happyShift action_6
action_22 (50) = happyShift action_7
action_22 (5) = happyGoto action_56
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (16) = happyShift action_3
action_23 (18) = happyShift action_4
action_23 (48) = happyShift action_5
action_23 (49) = happyShift action_6
action_23 (50) = happyShift action_7
action_23 (5) = happyGoto action_55
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (16) = happyShift action_3
action_24 (18) = happyShift action_4
action_24 (48) = happyShift action_5
action_24 (49) = happyShift action_6
action_24 (50) = happyShift action_7
action_24 (5) = happyGoto action_54
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (16) = happyShift action_3
action_25 (18) = happyShift action_4
action_25 (48) = happyShift action_5
action_25 (49) = happyShift action_6
action_25 (50) = happyShift action_7
action_25 (5) = happyGoto action_53
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (16) = happyShift action_3
action_26 (18) = happyShift action_4
action_26 (48) = happyShift action_5
action_26 (49) = happyShift action_6
action_26 (50) = happyShift action_7
action_26 (5) = happyGoto action_52
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (16) = happyShift action_3
action_27 (18) = happyShift action_4
action_27 (48) = happyShift action_5
action_27 (49) = happyShift action_6
action_27 (50) = happyShift action_7
action_27 (5) = happyGoto action_51
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (16) = happyShift action_3
action_28 (18) = happyShift action_4
action_28 (48) = happyShift action_5
action_28 (49) = happyShift action_6
action_28 (50) = happyShift action_7
action_28 (5) = happyGoto action_40
action_28 (6) = happyGoto action_50
action_28 _ = happyReduce_35

action_29 (16) = happyShift action_3
action_29 (18) = happyShift action_4
action_29 (48) = happyShift action_5
action_29 (49) = happyShift action_6
action_29 (50) = happyShift action_7
action_29 (5) = happyGoto action_40
action_29 (6) = happyGoto action_49
action_29 _ = happyReduce_35

action_30 (16) = happyShift action_3
action_30 (18) = happyShift action_4
action_30 (48) = happyShift action_5
action_30 (49) = happyShift action_6
action_30 (50) = happyShift action_7
action_30 (5) = happyGoto action_40
action_30 (6) = happyGoto action_48
action_30 _ = happyReduce_35

action_31 (16) = happyShift action_3
action_31 (18) = happyShift action_4
action_31 (48) = happyShift action_5
action_31 (49) = happyShift action_6
action_31 (50) = happyShift action_7
action_31 (5) = happyGoto action_40
action_31 (6) = happyGoto action_47
action_31 _ = happyReduce_35

action_32 (16) = happyShift action_3
action_32 (18) = happyShift action_4
action_32 (48) = happyShift action_5
action_32 (49) = happyShift action_6
action_32 (50) = happyShift action_7
action_32 (5) = happyGoto action_40
action_32 (6) = happyGoto action_46
action_32 _ = happyReduce_35

action_33 (16) = happyShift action_3
action_33 (18) = happyShift action_4
action_33 (48) = happyShift action_5
action_33 (49) = happyShift action_6
action_33 (50) = happyShift action_7
action_33 (5) = happyGoto action_40
action_33 (6) = happyGoto action_45
action_33 _ = happyReduce_35

action_34 (16) = happyShift action_3
action_34 (18) = happyShift action_4
action_34 (48) = happyShift action_5
action_34 (49) = happyShift action_6
action_34 (50) = happyShift action_7
action_34 (5) = happyGoto action_40
action_34 (6) = happyGoto action_44
action_34 _ = happyReduce_35

action_35 (16) = happyShift action_3
action_35 (18) = happyShift action_4
action_35 (48) = happyShift action_5
action_35 (49) = happyShift action_6
action_35 (50) = happyShift action_7
action_35 (5) = happyGoto action_40
action_35 (6) = happyGoto action_43
action_35 _ = happyReduce_35

action_36 (16) = happyShift action_3
action_36 (18) = happyShift action_4
action_36 (48) = happyShift action_5
action_36 (49) = happyShift action_6
action_36 (50) = happyShift action_7
action_36 (5) = happyGoto action_40
action_36 (6) = happyGoto action_42
action_36 _ = happyReduce_35

action_37 (16) = happyShift action_3
action_37 (18) = happyShift action_4
action_37 (48) = happyShift action_5
action_37 (49) = happyShift action_6
action_37 (50) = happyShift action_7
action_37 (5) = happyGoto action_40
action_37 (6) = happyGoto action_41
action_37 _ = happyReduce_35

action_38 (16) = happyShift action_3
action_38 (18) = happyShift action_4
action_38 (48) = happyShift action_5
action_38 (49) = happyShift action_6
action_38 (50) = happyShift action_7
action_38 (5) = happyGoto action_39
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (16) = happyShift action_3
action_39 (18) = happyShift action_4
action_39 (48) = happyShift action_5
action_39 (49) = happyShift action_6
action_39 (50) = happyShift action_7
action_39 (5) = happyGoto action_108
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (16) = happyShift action_3
action_40 (18) = happyShift action_4
action_40 (48) = happyShift action_5
action_40 (49) = happyShift action_6
action_40 (50) = happyShift action_7
action_40 (5) = happyGoto action_40
action_40 (6) = happyGoto action_107
action_40 _ = happyReduce_35

action_41 (17) = happyShift action_106
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (17) = happyShift action_105
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (17) = happyShift action_104
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (17) = happyShift action_103
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (17) = happyShift action_102
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (17) = happyShift action_101
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (17) = happyShift action_100
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (17) = happyShift action_99
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (17) = happyShift action_98
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (17) = happyShift action_97
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (16) = happyShift action_3
action_51 (18) = happyShift action_4
action_51 (48) = happyShift action_5
action_51 (49) = happyShift action_6
action_51 (50) = happyShift action_7
action_51 (5) = happyGoto action_96
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (17) = happyShift action_95
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (17) = happyShift action_94
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (17) = happyShift action_93
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (17) = happyShift action_92
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (17) = happyShift action_91
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (17) = happyShift action_90
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (17) = happyShift action_89
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (50) = happyShift action_88
action_59 (7) = happyGoto action_87
action_59 _ = happyReduce_37

action_60 (17) = happyShift action_85
action_60 (27) = happyShift action_86
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (16) = happyShift action_84
action_61 (14) = happyGoto action_82
action_61 (15) = happyGoto action_83
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (16) = happyShift action_3
action_62 (18) = happyShift action_4
action_62 (48) = happyShift action_5
action_62 (49) = happyShift action_6
action_62 (50) = happyShift action_7
action_62 (5) = happyGoto action_81
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (16) = happyShift action_3
action_63 (18) = happyShift action_4
action_63 (48) = happyShift action_5
action_63 (49) = happyShift action_6
action_63 (50) = happyShift action_7
action_63 (5) = happyGoto action_80
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (16) = happyShift action_3
action_64 (18) = happyShift action_4
action_64 (48) = happyShift action_5
action_64 (49) = happyShift action_6
action_64 (50) = happyShift action_7
action_64 (5) = happyGoto action_79
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (16) = happyShift action_78
action_65 (11) = happyGoto action_76
action_65 (12) = happyGoto action_77
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (16) = happyShift action_3
action_66 (18) = happyShift action_4
action_66 (48) = happyShift action_5
action_66 (49) = happyShift action_6
action_66 (50) = happyShift action_7
action_66 (5) = happyGoto action_75
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (16) = happyShift action_3
action_67 (18) = happyShift action_4
action_67 (48) = happyShift action_5
action_67 (49) = happyShift action_6
action_67 (50) = happyShift action_7
action_67 (5) = happyGoto action_74
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (17) = happyShift action_73
action_68 _ = happyFail (happyExpListPerState 68)

action_69 _ = happyReduce_23

action_70 _ = happyReduce_39

action_71 (16) = happyShift action_3
action_71 (18) = happyShift action_4
action_71 (48) = happyShift action_5
action_71 (49) = happyShift action_6
action_71 (50) = happyShift action_7
action_71 (5) = happyGoto action_9
action_71 (8) = happyGoto action_72
action_71 _ = happyFail (happyExpListPerState 71)

action_72 _ = happyReduce_40

action_73 _ = happyReduce_19

action_74 (17) = happyShift action_124
action_74 _ = happyFail (happyExpListPerState 74)

action_75 (17) = happyShift action_123
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (17) = happyShift action_122
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (16) = happyShift action_78
action_77 (11) = happyGoto action_121
action_77 (12) = happyGoto action_77
action_77 _ = happyReduce_44

action_78 (50) = happyShift action_120
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (17) = happyShift action_119
action_79 _ = happyFail (happyExpListPerState 79)

action_80 (16) = happyShift action_3
action_80 (18) = happyShift action_4
action_80 (48) = happyShift action_5
action_80 (49) = happyShift action_6
action_80 (50) = happyShift action_7
action_80 (5) = happyGoto action_118
action_80 _ = happyFail (happyExpListPerState 80)

action_81 (16) = happyShift action_3
action_81 (18) = happyShift action_4
action_81 (48) = happyShift action_5
action_81 (49) = happyShift action_6
action_81 (50) = happyShift action_7
action_81 (5) = happyGoto action_117
action_81 _ = happyFail (happyExpListPerState 81)

action_82 (17) = happyShift action_116
action_82 _ = happyFail (happyExpListPerState 82)

action_83 (16) = happyShift action_84
action_83 (14) = happyGoto action_115
action_83 (15) = happyGoto action_83
action_83 _ = happyReduce_48

action_84 (16) = happyShift action_3
action_84 (18) = happyShift action_4
action_84 (48) = happyShift action_5
action_84 (49) = happyShift action_6
action_84 (50) = happyShift action_7
action_84 (5) = happyGoto action_114
action_84 _ = happyFail (happyExpListPerState 84)

action_85 _ = happyReduce_29

action_86 (16) = happyShift action_3
action_86 (18) = happyShift action_4
action_86 (48) = happyShift action_5
action_86 (49) = happyShift action_6
action_86 (50) = happyShift action_7
action_86 (5) = happyGoto action_113
action_86 _ = happyFail (happyExpListPerState 86)

action_87 (17) = happyShift action_112
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (50) = happyShift action_88
action_88 (7) = happyGoto action_111
action_88 _ = happyReduce_37

action_89 _ = happyReduce_20

action_90 _ = happyReduce_21

action_91 _ = happyReduce_25

action_92 _ = happyReduce_26

action_93 _ = happyReduce_15

action_94 _ = happyReduce_16

action_95 _ = happyReduce_17

action_96 (17) = happyShift action_110
action_96 _ = happyFail (happyExpListPerState 96)

action_97 _ = happyReduce_5

action_98 _ = happyReduce_6

action_99 _ = happyReduce_7

action_100 _ = happyReduce_8

action_101 _ = happyReduce_9

action_102 _ = happyReduce_10

action_103 _ = happyReduce_11

action_104 _ = happyReduce_12

action_105 _ = happyReduce_13

action_106 _ = happyReduce_14

action_107 _ = happyReduce_36

action_108 (17) = happyShift action_109
action_108 _ = happyFail (happyExpListPerState 108)

action_109 _ = happyReduce_22

action_110 _ = happyReduce_18

action_111 _ = happyReduce_38

action_112 (16) = happyShift action_3
action_112 (18) = happyShift action_4
action_112 (48) = happyShift action_5
action_112 (49) = happyShift action_6
action_112 (50) = happyShift action_7
action_112 (5) = happyGoto action_130
action_112 _ = happyFail (happyExpListPerState 112)

action_113 (17) = happyShift action_129
action_113 _ = happyFail (happyExpListPerState 113)

action_114 (16) = happyShift action_3
action_114 (18) = happyShift action_4
action_114 (48) = happyShift action_5
action_114 (49) = happyShift action_6
action_114 (50) = happyShift action_7
action_114 (5) = happyGoto action_128
action_114 _ = happyFail (happyExpListPerState 114)

action_115 _ = happyReduce_47

action_116 _ = happyReduce_46

action_117 (17) = happyShift action_127
action_117 _ = happyFail (happyExpListPerState 117)

action_118 (17) = happyShift action_126
action_118 _ = happyFail (happyExpListPerState 118)

action_119 _ = happyReduce_33

action_120 (16) = happyShift action_3
action_120 (18) = happyShift action_4
action_120 (48) = happyShift action_5
action_120 (49) = happyShift action_6
action_120 (50) = happyShift action_7
action_120 (5) = happyGoto action_125
action_120 _ = happyFail (happyExpListPerState 120)

action_121 _ = happyReduce_43

action_122 _ = happyReduce_42

action_123 _ = happyReduce_32

action_124 _ = happyReduce_31

action_125 (17) = happyShift action_133
action_125 _ = happyFail (happyExpListPerState 125)

action_126 _ = happyReduce_27

action_127 _ = happyReduce_28

action_128 (17) = happyShift action_132
action_128 _ = happyFail (happyExpListPerState 128)

action_129 _ = happyReduce_30

action_130 (17) = happyShift action_131
action_130 _ = happyFail (happyExpListPerState 130)

action_131 _ = happyReduce_34

action_132 _ = happyReduce_49

action_133 _ = happyReduce_45

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyTerminal (TOKENENTERO happy_var_1))
	 =  HappyAbsSyn5
		 (NumS happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  5 happyReduction_3
happyReduction_3 (HappyTerminal (TOKENBOOLEANO happy_var_1))
	 =  HappyAbsSyn5
		 (BoolS happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  5 happyReduction_4
happyReduction_4 (HappyTerminal (TOKENIDENTIFICADOR happy_var_1))
	 =  HappyAbsSyn5
		 (VarS happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happyReduce 4 5 happyReduction_5
happyReduction_5 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (AddS happy_var_3
	) `HappyStk` happyRest

happyReduce_6 = happyReduce 4 5 happyReduction_6
happyReduction_6 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (SubS happy_var_3
	) `HappyStk` happyRest

happyReduce_7 = happyReduce 4 5 happyReduction_7
happyReduction_7 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (MulS happy_var_3
	) `HappyStk` happyRest

happyReduce_8 = happyReduce 4 5 happyReduction_8
happyReduction_8 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (DivS happy_var_3
	) `HappyStk` happyRest

happyReduce_9 = happyReduce 4 5 happyReduction_9
happyReduction_9 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (EqS happy_var_3
	) `HappyStk` happyRest

happyReduce_10 = happyReduce 4 5 happyReduction_10
happyReduction_10 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (LtS happy_var_3
	) `HappyStk` happyRest

happyReduce_11 = happyReduce 4 5 happyReduction_11
happyReduction_11 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (GtS happy_var_3
	) `HappyStk` happyRest

happyReduce_12 = happyReduce 4 5 happyReduction_12
happyReduction_12 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (LeqS happy_var_3
	) `HappyStk` happyRest

happyReduce_13 = happyReduce 4 5 happyReduction_13
happyReduction_13 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (GeqS happy_var_3
	) `HappyStk` happyRest

happyReduce_14 = happyReduce 4 5 happyReduction_14
happyReduction_14 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (NeqS happy_var_3
	) `HappyStk` happyRest

happyReduce_15 = happyReduce 4 5 happyReduction_15
happyReduction_15 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (Add1S happy_var_3
	) `HappyStk` happyRest

happyReduce_16 = happyReduce 4 5 happyReduction_16
happyReduction_16 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (Sub1S happy_var_3
	) `HappyStk` happyRest

happyReduce_17 = happyReduce 4 5 happyReduction_17
happyReduction_17 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (SqrtS happy_var_3
	) `HappyStk` happyRest

happyReduce_18 = happyReduce 5 5 happyReduction_18
happyReduction_18 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (ExptS happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_19 = happyReduce 4 5 happyReduction_19
happyReduction_19 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (AppS happy_var_2 happy_var_3
	) `HappyStk` happyRest

happyReduce_20 = happyReduce 4 5 happyReduction_20
happyReduction_20 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (FstS happy_var_3
	) `HappyStk` happyRest

happyReduce_21 = happyReduce 4 5 happyReduction_21
happyReduction_21 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (SndS happy_var_3
	) `HappyStk` happyRest

happyReduce_22 = happyReduce 5 5 happyReduction_22
happyReduction_22 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (PairS happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_23 = happySpecReduce_3  5 happyReduction_23
happyReduction_23 _
	(HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (ListS happy_var_2
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_2  5 happyReduction_24
happyReduction_24 _
	_
	 =  HappyAbsSyn5
		 (ListS []
	)

happyReduce_25 = happyReduce 4 5 happyReduction_25
happyReduction_25 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (HeadS happy_var_3
	) `HappyStk` happyRest

happyReduce_26 = happyReduce 4 5 happyReduction_26
happyReduction_26 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (TailS happy_var_3
	) `HappyStk` happyRest

happyReduce_27 = happyReduce 6 5 happyReduction_27
happyReduction_27 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_5) `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (IfS happy_var_3 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_28 = happyReduce 6 5 happyReduction_28
happyReduction_28 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_5) `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (If0S happy_var_3 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_29 = happyReduce 4 5 happyReduction_29
happyReduction_29 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (CondS happy_var_3 Nothing
	) `HappyStk` happyRest

happyReduce_30 = happyReduce 6 5 happyReduction_30
happyReduction_30 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (CondS happy_var_3 (Just happy_var_5)
	) `HappyStk` happyRest

happyReduce_31 = happyReduce 5 5 happyReduction_31
happyReduction_31 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (LetS happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_32 = happyReduce 5 5 happyReduction_32
happyReduction_32 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (LetRecS happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_33 = happyReduce 5 5 happyReduction_33
happyReduction_33 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (LetStarS happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_34 = happyReduce 7 5 happyReduction_34
happyReduction_34 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (LambdaS happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_35 = happySpecReduce_0  6 happyReduction_35
happyReduction_35  =  HappyAbsSyn6
		 ([]
	)

happyReduce_36 = happySpecReduce_2  6 happyReduction_36
happyReduction_36 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1 : happy_var_2
	)
happyReduction_36 _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_0  7 happyReduction_37
happyReduction_37  =  HappyAbsSyn7
		 ([]
	)

happyReduce_38 = happySpecReduce_2  7 happyReduction_38
happyReduction_38 (HappyAbsSyn7  happy_var_2)
	(HappyTerminal (TOKENIDENTIFICADOR happy_var_1))
	 =  HappyAbsSyn7
		 (happy_var_1 : happy_var_2
	)
happyReduction_38 _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_2  8 happyReduction_39
happyReduction_39 (HappyAbsSyn9  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1 : happy_var_2
	)
happyReduction_39 _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_2  9 happyReduction_40
happyReduction_40 (HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (happy_var_2
	)
happyReduction_40 _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_0  9 happyReduction_41
happyReduction_41  =  HappyAbsSyn9
		 ([]
	)

happyReduce_42 = happySpecReduce_3  10 happyReduction_42
happyReduction_42 _
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (happy_var_2
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_2  11 happyReduction_43
happyReduction_43 (HappyAbsSyn11  happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 : happy_var_2
	)
happyReduction_43 _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  11 happyReduction_44
happyReduction_44 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 ([happy_var_1]
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happyReduce 4 12 happyReduction_45
happyReduction_45 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	(HappyTerminal (TOKENIDENTIFICADOR happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 ((happy_var_2, happy_var_3)
	) `HappyStk` happyRest

happyReduce_46 = happySpecReduce_3  13 happyReduction_46
happyReduction_46 _
	(HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (happy_var_2
	)
happyReduction_46 _ _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_2  14 happyReduction_47
happyReduction_47 (HappyAbsSyn14  happy_var_2)
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1 : happy_var_2
	)
happyReduction_47 _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_1  14 happyReduction_48
happyReduction_48 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn14
		 ([happy_var_1]
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happyReduce 4 15 happyReduction_49
happyReduction_49 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 ((happy_var_2, happy_var_3)
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 51 51 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TOKENPARENTESISIZQUIERDO -> cont 16;
	TOKENPARENTESISDERECHO -> cont 17;
	TOKENCORCHETEIZQUIERDO -> cont 18;
	TOKENCORCHETEDERECHO -> cont 19;
	TOKENCOMA -> cont 20;
	TOKENLET -> cont 21;
	TOKENLETREC -> cont 22;
	TOKENLETESTRELLA -> cont 23;
	TOKENIF -> cont 24;
	TOKENIFCERO -> cont 25;
	TOKENCOND -> cont 26;
	TOKENELSE -> cont 27;
	TOKENLAMBDA -> cont 28;
	TOKENPRIMERO -> cont 29;
	TOKENSEGUNDO -> cont 30;
	TOKENCABEZA -> cont 31;
	TOKENCOLA -> cont 32;
	TOKENMASUNO -> cont 33;
	TOKENMENOSUNO -> cont 34;
	TOKENRAIZCUADRADA -> cont 35;
	TOKENPOTENCIA -> cont 36;
	TOKENSUMA -> cont 37;
	TOKENRESTA -> cont 38;
	TOKENMULTIPLICACION -> cont 39;
	TOKENDIVISION -> cont 40;
	TOKENIGUAL -> cont 41;
	TOKENMENORQUE -> cont 42;
	TOKENMAYORQUE -> cont 43;
	TOKENMENORIGUALQUE -> cont 44;
	TOKENMAYORIGUALQUE -> cont 45;
	TOKENDIFERENTEQUE -> cont 46;
	TOKENPAIR -> cont 47;
	TOKENBOOLEANO happy_dollar_dollar -> cont 48;
	TOKENENTERO happy_dollar_dollar -> cont 49;
	TOKENIDENTIFICADOR happy_dollar_dollar -> cont 50;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 51 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Prelude.Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Prelude.Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> HappyIdentity a
happyError' = HappyIdentity Prelude.. (\(tokens, _) -> parseError tokens)
parse tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


-- Error de parseo (muere con mensaje)
parseError :: [Token] -> a
parseError _ = error "Parse error"
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
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
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
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





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









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
