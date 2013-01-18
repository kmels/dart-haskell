{-# OPTIONS_GHC -w #-}
{-# LANGUAGE BangPatterns #-} -- required for versions of Happy before 1.18.6
{-# OPTIONS -Wwarn -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module ParserCore ( parseCore ) where

import IfaceSyn
import ForeignCall
import RdrHsSyn
import HsSyn
import RdrName
import OccName
import TypeRep ( TyThing(..) )
import Type ( Kind,
              liftedTypeKindTyCon, openTypeKindTyCon, unliftedTypeKindTyCon,
              mkTyConApp
            )
import Kind( mkArrowKind )
import Name( Name, nameOccName, nameModule, mkExternalName, wiredInNameTyThing_maybe )
import Module
import ParserCoreUtils
import LexCore
import Literal
import SrcLoc
import PrelNames
import TysPrim
import TyCon ( TyCon, tyConName )
import FastString
import Outputable
import Data.Char
import Unique

#include "../HsVersions.h"

-- parser produced by Happy Version 1.18.9

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 (HsExtCore RdrName)
	| HappyAbsSyn5 (Module)
	| HappyAbsSyn6 (Name)
	| HappyAbsSyn9 ([String])
	| HappyAbsSyn11 ([TyClDecl RdrName])
	| HappyAbsSyn12 (TyClDecl RdrName)
	| HappyAbsSyn13 (OccName -> [LConDecl RdrName])
	| HappyAbsSyn14 ([LConDecl RdrName])
	| HappyAbsSyn15 (LConDecl RdrName)
	| HappyAbsSyn16 ([LHsTyVarBndr RdrName])
	| HappyAbsSyn17 ([LHsType RdrName])
	| HappyAbsSyn18 ([IfaceType])
	| HappyAbsSyn19 (IfaceType)
	| HappyAbsSyn22 ([IfaceBinding])
	| HappyAbsSyn23 (IfaceBinding)
	| HappyAbsSyn24 ([(IfaceLetBndr, IfaceExpr)])
	| HappyAbsSyn25 ((IfaceLetBndr, IfaceExpr))
	| HappyAbsSyn26 (IfaceBndr)
	| HappyAbsSyn27 ([IfaceBndr])
	| HappyAbsSyn28 (IfaceIdBndr)
	| HappyAbsSyn29 (IfaceTvBndr)
	| HappyAbsSyn30 ([IfaceTvBndr])
	| HappyAbsSyn31 (IfaceKind)
	| HappyAbsSyn33 (IfaceExpr)
	| HappyAbsSyn36 ([IfaceAlt])
	| HappyAbsSyn37 (IfaceAlt)
	| HappyAbsSyn38 (Literal)
	| HappyAbsSyn39 (FastString)
	| HappyAbsSyn40 (String)
	| HappyAbsSyn41 (OccName)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> m HappyAbsSyn
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
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110,
 action_111,
 action_112,
 action_113,
 action_114,
 action_115,
 action_116,
 action_117,
 action_118,
 action_119,
 action_120,
 action_121,
 action_122,
 action_123,
 action_124,
 action_125,
 action_126,
 action_127,
 action_128,
 action_129,
 action_130,
 action_131,
 action_132,
 action_133,
 action_134,
 action_135,
 action_136,
 action_137,
 action_138,
 action_139,
 action_140,
 action_141,
 action_142,
 action_143,
 action_144,
 action_145,
 action_146,
 action_147,
 action_148,
 action_149,
 action_150,
 action_151,
 action_152,
 action_153,
 action_154,
 action_155,
 action_156,
 action_157,
 action_158,
 action_159,
 action_160,
 action_161,
 action_162,
 action_163,
 action_164,
 action_165,
 action_166,
 action_167,
 action_168,
 action_169,
 action_170,
 action_171,
 action_172,
 action_173,
 action_174,
 action_175,
 action_176,
 action_177,
 action_178,
 action_179,
 action_180,
 action_181,
 action_182,
 action_183,
 action_184,
 action_185,
 action_186,
 action_187,
 action_188,
 action_189,
 action_190,
 action_191,
 action_192,
 action_193,
 action_194 :: () => Int -> ({-HappyReduction (P) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> (P) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> (P) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (P) HappyAbsSyn)

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
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51,
 happyReduce_52,
 happyReduce_53,
 happyReduce_54,
 happyReduce_55,
 happyReduce_56,
 happyReduce_57,
 happyReduce_58,
 happyReduce_59,
 happyReduce_60,
 happyReduce_61,
 happyReduce_62,
 happyReduce_63,
 happyReduce_64,
 happyReduce_65,
 happyReduce_66,
 happyReduce_67,
 happyReduce_68,
 happyReduce_69,
 happyReduce_70,
 happyReduce_71,
 happyReduce_72,
 happyReduce_73,
 happyReduce_74,
 happyReduce_75,
 happyReduce_76,
 happyReduce_77,
 happyReduce_78,
 happyReduce_79,
 happyReduce_80,
 happyReduce_81,
 happyReduce_82,
 happyReduce_83,
 happyReduce_84 :: () => ({-HappyReduction (P) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> (P) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> (P) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (P) HappyAbsSyn)

action_0 (42) = happyShift action_2
action_0 (4) = happyGoto action_3
action_0 _ = happyFail

action_1 (42) = happyShift action_2
action_1 _ = happyFail

action_2 (72) = happyShift action_5
action_2 (5) = happyGoto action_4
action_2 _ = happyFail

action_3 (78) = happyAccept
action_3 _ = happyFail

action_4 (43) = happyShift action_9
action_4 (44) = happyShift action_10
action_4 (11) = happyGoto action_7
action_4 (12) = happyGoto action_8
action_4 _ = happyReduce_10

action_5 (62) = happyShift action_6
action_5 _ = happyFail

action_6 (73) = happyShift action_23
action_6 (9) = happyGoto action_22
action_6 _ = happyFail

action_7 (46) = happyShift action_19
action_7 (54) = happyShift action_20
action_7 (72) = happyShift action_21
action_7 (22) = happyGoto action_15
action_7 (23) = happyGoto action_16
action_7 (25) = happyGoto action_17
action_7 (39) = happyGoto action_18
action_7 _ = happyReduce_35

action_8 (43) = happyShift action_9
action_8 (44) = happyShift action_10
action_8 (11) = happyGoto action_14
action_8 (12) = happyGoto action_8
action_8 _ = happyReduce_10

action_9 (72) = happyShift action_12
action_9 (7) = happyGoto action_13
action_9 _ = happyFail

action_10 (72) = happyShift action_12
action_10 (7) = happyGoto action_11
action_10 _ = happyFail

action_11 (56) = happyShift action_32
action_11 (72) = happyShift action_21
action_11 (29) = happyGoto action_29
action_11 (30) = happyGoto action_34
action_11 (39) = happyGoto action_31
action_11 _ = happyReduce_50

action_12 (62) = happyShift action_33
action_12 _ = happyFail

action_13 (56) = happyShift action_32
action_13 (72) = happyShift action_21
action_13 (29) = happyGoto action_29
action_13 (30) = happyGoto action_30
action_13 (39) = happyGoto action_31
action_13 _ = happyReduce_50

action_14 _ = happyReduce_11

action_15 _ = happyReduce_1

action_16 (71) = happyShift action_28
action_16 _ = happyFail

action_17 _ = happyReduce_38

action_18 (63) = happyShift action_27
action_18 _ = happyFail

action_19 (58) = happyShift action_26
action_19 _ = happyFail

action_20 (54) = happyShift action_20
action_20 (72) = happyShift action_21
action_20 (25) = happyGoto action_25
action_20 (39) = happyGoto action_18
action_20 _ = happyFail

action_21 _ = happyReduce_82

action_22 _ = happyReduce_2

action_23 (69) = happyShift action_24
action_23 _ = happyReduce_6

action_24 (73) = happyShift action_23
action_24 (9) = happyGoto action_52
action_24 _ = happyFail

action_25 _ = happyReduce_42

action_26 (54) = happyShift action_20
action_26 (72) = happyShift action_21
action_26 (24) = happyGoto action_50
action_26 (25) = happyGoto action_51
action_26 (39) = happyGoto action_18
action_26 _ = happyFail

action_27 (45) = happyShift action_47
action_27 (56) = happyShift action_48
action_27 (72) = happyShift action_49
action_27 (7) = happyGoto action_42
action_27 (8) = happyGoto action_43
action_27 (20) = happyGoto action_44
action_27 (21) = happyGoto action_45
action_27 (39) = happyGoto action_46
action_27 _ = happyFail

action_28 (46) = happyShift action_19
action_28 (54) = happyShift action_20
action_28 (72) = happyShift action_21
action_28 (22) = happyGoto action_41
action_28 (23) = happyGoto action_16
action_28 (25) = happyGoto action_17
action_28 (39) = happyGoto action_18
action_28 _ = happyReduce_35

action_29 (56) = happyShift action_32
action_29 (72) = happyShift action_21
action_29 (29) = happyGoto action_29
action_29 (30) = happyGoto action_40
action_29 (39) = happyGoto action_31
action_29 _ = happyReduce_50

action_30 (61) = happyShift action_39
action_30 _ = happyFail

action_31 _ = happyReduce_48

action_32 (72) = happyShift action_21
action_32 (39) = happyGoto action_38
action_32 _ = happyFail

action_33 (73) = happyShift action_23
action_33 (9) = happyGoto action_37
action_33 _ = happyFail

action_34 (61) = happyShift action_36
action_34 (13) = happyGoto action_35
action_34 _ = happyReduce_14

action_35 (71) = happyShift action_71
action_35 _ = happyFail

action_36 (45) = happyShift action_47
action_36 (56) = happyShift action_48
action_36 (72) = happyShift action_49
action_36 (7) = happyGoto action_42
action_36 (8) = happyGoto action_43
action_36 (20) = happyGoto action_44
action_36 (21) = happyGoto action_70
action_36 (39) = happyGoto action_46
action_36 _ = happyFail

action_37 _ = happyReduce_4

action_38 (63) = happyShift action_69
action_38 _ = happyFail

action_39 (58) = happyShift action_68
action_39 _ = happyFail

action_40 _ = happyReduce_51

action_41 _ = happyReduce_36

action_42 (56) = happyShift action_62
action_42 (72) = happyShift action_63
action_42 (7) = happyGoto action_58
action_42 (18) = happyGoto action_67
action_42 (19) = happyGoto action_60
action_42 (39) = happyGoto action_61
action_42 _ = happyReduce_23

action_43 (56) = happyShift action_62
action_43 (72) = happyShift action_63
action_43 (7) = happyGoto action_58
action_43 (18) = happyGoto action_66
action_43 (19) = happyGoto action_60
action_43 (39) = happyGoto action_61
action_43 _ = happyReduce_23

action_44 (66) = happyShift action_65
action_44 _ = happyReduce_32

action_45 (61) = happyShift action_64
action_45 _ = happyFail

action_46 (56) = happyShift action_62
action_46 (72) = happyShift action_63
action_46 (7) = happyGoto action_58
action_46 (18) = happyGoto action_59
action_46 (19) = happyGoto action_60
action_46 (39) = happyGoto action_61
action_46 _ = happyReduce_23

action_47 (56) = happyShift action_32
action_47 (72) = happyShift action_21
action_47 (29) = happyGoto action_29
action_47 (30) = happyGoto action_57
action_47 (39) = happyGoto action_31
action_47 _ = happyReduce_50

action_48 (45) = happyShift action_47
action_48 (56) = happyShift action_48
action_48 (72) = happyShift action_49
action_48 (7) = happyGoto action_42
action_48 (8) = happyGoto action_43
action_48 (20) = happyGoto action_44
action_48 (21) = happyGoto action_56
action_48 (39) = happyGoto action_46
action_48 _ = happyFail

action_49 (62) = happyShift action_55
action_49 _ = happyReduce_82

action_50 (59) = happyShift action_54
action_50 _ = happyFail

action_51 (71) = happyShift action_53
action_51 _ = happyReduce_39

action_52 _ = happyReduce_7

action_53 (54) = happyShift action_20
action_53 (72) = happyShift action_21
action_53 (24) = happyGoto action_104
action_53 (25) = happyGoto action_51
action_53 (39) = happyGoto action_18
action_53 _ = happyFail

action_54 _ = happyReduce_37

action_55 (72) = happyShift action_102
action_55 (73) = happyShift action_103
action_55 (9) = happyGoto action_37
action_55 (10) = happyGoto action_100
action_55 (40) = happyGoto action_101
action_55 _ = happyFail

action_56 (57) = happyShift action_99
action_56 _ = happyFail

action_57 (69) = happyShift action_98
action_57 _ = happyFail

action_58 _ = happyReduce_26

action_59 _ = happyReduce_28

action_60 (56) = happyShift action_62
action_60 (72) = happyShift action_63
action_60 (7) = happyGoto action_58
action_60 (18) = happyGoto action_97
action_60 (19) = happyGoto action_60
action_60 (39) = happyGoto action_61
action_60 _ = happyReduce_23

action_61 _ = happyReduce_25

action_62 (45) = happyShift action_47
action_62 (56) = happyShift action_48
action_62 (72) = happyShift action_49
action_62 (7) = happyGoto action_42
action_62 (8) = happyGoto action_43
action_62 (20) = happyGoto action_44
action_62 (21) = happyGoto action_96
action_62 (39) = happyGoto action_46
action_62 _ = happyFail

action_63 (62) = happyShift action_33
action_63 _ = happyReduce_82

action_64 (47) = happyShift action_89
action_64 (49) = happyShift action_90
action_64 (51) = happyShift action_91
action_64 (53) = happyShift action_92
action_64 (56) = happyShift action_93
action_64 (67) = happyShift action_94
action_64 (72) = happyShift action_95
action_64 (6) = happyGoto action_82
action_64 (8) = happyGoto action_83
action_64 (33) = happyGoto action_84
action_64 (34) = happyGoto action_85
action_64 (35) = happyGoto action_86
action_64 (38) = happyGoto action_87
action_64 (39) = happyGoto action_88
action_64 _ = happyFail

action_65 (45) = happyShift action_47
action_65 (56) = happyShift action_48
action_65 (72) = happyShift action_49
action_65 (7) = happyGoto action_42
action_65 (8) = happyGoto action_43
action_65 (20) = happyGoto action_44
action_65 (21) = happyGoto action_81
action_65 (39) = happyGoto action_46
action_65 _ = happyFail

action_66 _ = happyReduce_29

action_67 _ = happyReduce_30

action_68 (73) = happyShift action_80
action_68 (14) = happyGoto action_77
action_68 (15) = happyGoto action_78
action_68 (41) = happyGoto action_79
action_68 _ = happyReduce_16

action_69 (56) = happyShift action_73
action_69 (60) = happyShift action_74
action_69 (65) = happyShift action_75
action_69 (70) = happyShift action_76
action_69 (31) = happyGoto action_72
action_69 _ = happyFail

action_70 _ = happyReduce_15

action_71 _ = happyReduce_13

action_72 (57) = happyShift action_131
action_72 _ = happyFail

action_73 (56) = happyShift action_73
action_73 (60) = happyShift action_74
action_73 (65) = happyShift action_75
action_73 (70) = happyShift action_76
action_73 (31) = happyGoto action_129
action_73 (32) = happyGoto action_130
action_73 _ = happyFail

action_74 _ = happyReduce_53

action_75 _ = happyReduce_52

action_76 _ = happyReduce_54

action_77 (59) = happyShift action_128
action_77 _ = happyFail

action_78 (71) = happyShift action_127
action_78 _ = happyReduce_17

action_79 (68) = happyShift action_126
action_79 (16) = happyGoto action_125
action_79 _ = happyReduce_20

action_80 _ = happyReduce_84

action_81 _ = happyReduce_33

action_82 _ = happyReduce_60

action_83 _ = happyReduce_59

action_84 _ = happyReduce_65

action_85 (56) = happyShift action_93
action_85 (68) = happyShift action_124
action_85 (72) = happyShift action_95
action_85 (6) = happyGoto action_82
action_85 (8) = happyGoto action_83
action_85 (33) = happyGoto action_123
action_85 (38) = happyGoto action_87
action_85 (39) = happyGoto action_88
action_85 _ = happyReduce_66

action_86 _ = happyReduce_41

action_87 _ = happyReduce_61

action_88 _ = happyReduce_58

action_89 (46) = happyShift action_19
action_89 (54) = happyShift action_20
action_89 (72) = happyShift action_21
action_89 (23) = happyGoto action_122
action_89 (25) = happyGoto action_17
action_89 (39) = happyGoto action_18
action_89 _ = happyFail

action_90 (56) = happyShift action_121
action_90 _ = happyFail

action_91 (56) = happyShift action_93
action_91 (72) = happyShift action_95
action_91 (6) = happyGoto action_82
action_91 (8) = happyGoto action_83
action_91 (33) = happyGoto action_120
action_91 (38) = happyGoto action_87
action_91 (39) = happyGoto action_88
action_91 _ = happyFail

action_92 (76) = happyShift action_119
action_92 _ = happyFail

action_93 (47) = happyShift action_89
action_93 (49) = happyShift action_90
action_93 (51) = happyShift action_91
action_93 (53) = happyShift action_92
action_93 (56) = happyShift action_93
action_93 (67) = happyShift action_94
action_93 (72) = happyShift action_95
action_93 (74) = happyShift action_115
action_93 (75) = happyShift action_116
action_93 (76) = happyShift action_117
action_93 (77) = happyShift action_118
action_93 (6) = happyGoto action_82
action_93 (8) = happyGoto action_83
action_93 (33) = happyGoto action_84
action_93 (34) = happyGoto action_85
action_93 (35) = happyGoto action_114
action_93 (38) = happyGoto action_87
action_93 (39) = happyGoto action_88
action_93 _ = happyFail

action_94 (56) = happyShift action_112
action_94 (68) = happyShift action_113
action_94 (26) = happyGoto action_109
action_94 (27) = happyGoto action_110
action_94 (28) = happyGoto action_111
action_94 _ = happyFail

action_95 (62) = happyShift action_108
action_95 _ = happyReduce_82

action_96 (57) = happyShift action_107
action_96 _ = happyFail

action_97 _ = happyReduce_24

action_98 (45) = happyShift action_47
action_98 (56) = happyShift action_48
action_98 (72) = happyShift action_49
action_98 (7) = happyGoto action_42
action_98 (8) = happyGoto action_43
action_98 (20) = happyGoto action_44
action_98 (21) = happyGoto action_106
action_98 (39) = happyGoto action_46
action_98 _ = happyFail

action_99 _ = happyReduce_31

action_100 _ = happyReduce_5

action_101 _ = happyReduce_8

action_102 _ = happyReduce_83

action_103 (69) = happyShift action_105
action_103 _ = happyReduce_6

action_104 _ = happyReduce_40

action_105 (72) = happyShift action_102
action_105 (73) = happyShift action_103
action_105 (9) = happyGoto action_52
action_105 (10) = happyGoto action_154
action_105 (40) = happyGoto action_101
action_105 _ = happyFail

action_106 _ = happyReduce_34

action_107 _ = happyReduce_27

action_108 (72) = happyShift action_102
action_108 (73) = happyShift action_103
action_108 (9) = happyGoto action_153
action_108 (10) = happyGoto action_100
action_108 (40) = happyGoto action_101
action_108 _ = happyFail

action_109 (56) = happyShift action_112
action_109 (68) = happyShift action_113
action_109 (26) = happyGoto action_109
action_109 (27) = happyGoto action_152
action_109 (28) = happyGoto action_111
action_109 _ = happyReduce_45

action_110 (66) = happyShift action_151
action_110 _ = happyFail

action_111 _ = happyReduce_44

action_112 (72) = happyShift action_21
action_112 (39) = happyGoto action_150
action_112 _ = happyFail

action_113 (56) = happyShift action_32
action_113 (72) = happyShift action_21
action_113 (29) = happyGoto action_149
action_113 (39) = happyGoto action_31
action_113 _ = happyFail

action_114 (57) = happyShift action_148
action_114 _ = happyFail

action_115 (63) = happyShift action_147
action_115 _ = happyFail

action_116 (63) = happyShift action_146
action_116 _ = happyFail

action_117 (63) = happyShift action_145
action_117 _ = happyFail

action_118 (63) = happyShift action_144
action_118 _ = happyFail

action_119 (56) = happyShift action_62
action_119 (72) = happyShift action_63
action_119 (7) = happyGoto action_58
action_119 (19) = happyGoto action_143
action_119 (39) = happyGoto action_61
action_119 _ = happyFail

action_120 (56) = happyShift action_62
action_120 (72) = happyShift action_63
action_120 (7) = happyGoto action_58
action_120 (19) = happyGoto action_142
action_120 (39) = happyGoto action_61
action_120 _ = happyFail

action_121 (45) = happyShift action_47
action_121 (56) = happyShift action_48
action_121 (72) = happyShift action_49
action_121 (7) = happyGoto action_42
action_121 (8) = happyGoto action_43
action_121 (20) = happyGoto action_44
action_121 (21) = happyGoto action_141
action_121 (39) = happyGoto action_46
action_121 _ = happyFail

action_122 (48) = happyShift action_140
action_122 _ = happyFail

action_123 _ = happyReduce_63

action_124 (56) = happyShift action_62
action_124 (72) = happyShift action_63
action_124 (7) = happyGoto action_58
action_124 (19) = happyGoto action_139
action_124 (39) = happyGoto action_61
action_124 _ = happyFail

action_125 (56) = happyShift action_62
action_125 (72) = happyShift action_63
action_125 (7) = happyGoto action_58
action_125 (17) = happyGoto action_137
action_125 (18) = happyGoto action_138
action_125 (19) = happyGoto action_60
action_125 (39) = happyGoto action_61
action_125 _ = happyReduce_23

action_126 (56) = happyShift action_32
action_126 (72) = happyShift action_21
action_126 (29) = happyGoto action_136
action_126 (39) = happyGoto action_31
action_126 _ = happyFail

action_127 (73) = happyShift action_80
action_127 (14) = happyGoto action_135
action_127 (15) = happyGoto action_78
action_127 (41) = happyGoto action_79
action_127 _ = happyReduce_16

action_128 (71) = happyShift action_134
action_128 _ = happyFail

action_129 (66) = happyShift action_133
action_129 _ = happyReduce_56

action_130 (57) = happyShift action_132
action_130 _ = happyFail

action_131 _ = happyReduce_49

action_132 _ = happyReduce_55

action_133 (56) = happyShift action_73
action_133 (60) = happyShift action_74
action_133 (65) = happyShift action_75
action_133 (70) = happyShift action_76
action_133 (31) = happyGoto action_129
action_133 (32) = happyGoto action_164
action_133 _ = happyFail

action_134 _ = happyReduce_12

action_135 _ = happyReduce_18

action_136 (68) = happyShift action_126
action_136 (16) = happyGoto action_163
action_136 _ = happyReduce_20

action_137 _ = happyReduce_19

action_138 _ = happyReduce_22

action_139 _ = happyReduce_64

action_140 (47) = happyShift action_89
action_140 (49) = happyShift action_90
action_140 (51) = happyShift action_91
action_140 (53) = happyShift action_92
action_140 (56) = happyShift action_93
action_140 (67) = happyShift action_94
action_140 (72) = happyShift action_95
action_140 (6) = happyGoto action_82
action_140 (8) = happyGoto action_83
action_140 (33) = happyGoto action_84
action_140 (34) = happyGoto action_85
action_140 (35) = happyGoto action_162
action_140 (38) = happyGoto action_87
action_140 (39) = happyGoto action_88
action_140 _ = happyFail

action_141 (57) = happyShift action_161
action_141 _ = happyFail

action_142 _ = happyReduce_70

action_143 _ = happyReduce_71

action_144 (56) = happyShift action_62
action_144 (72) = happyShift action_63
action_144 (7) = happyGoto action_58
action_144 (19) = happyGoto action_160
action_144 (39) = happyGoto action_61
action_144 _ = happyFail

action_145 (56) = happyShift action_62
action_145 (72) = happyShift action_63
action_145 (7) = happyGoto action_58
action_145 (19) = happyGoto action_159
action_145 (39) = happyGoto action_61
action_145 _ = happyFail

action_146 (56) = happyShift action_62
action_146 (72) = happyShift action_63
action_146 (7) = happyGoto action_58
action_146 (19) = happyGoto action_158
action_146 (39) = happyGoto action_61
action_146 _ = happyFail

action_147 (56) = happyShift action_62
action_147 (72) = happyShift action_63
action_147 (7) = happyGoto action_58
action_147 (19) = happyGoto action_157
action_147 (39) = happyGoto action_61
action_147 _ = happyFail

action_148 _ = happyReduce_62

action_149 _ = happyReduce_43

action_150 (63) = happyShift action_156
action_150 _ = happyFail

action_151 (47) = happyShift action_89
action_151 (49) = happyShift action_90
action_151 (51) = happyShift action_91
action_151 (53) = happyShift action_92
action_151 (56) = happyShift action_93
action_151 (67) = happyShift action_94
action_151 (72) = happyShift action_95
action_151 (6) = happyGoto action_82
action_151 (8) = happyGoto action_83
action_151 (33) = happyGoto action_84
action_151 (34) = happyGoto action_85
action_151 (35) = happyGoto action_155
action_151 (38) = happyGoto action_87
action_151 (39) = happyGoto action_88
action_151 _ = happyFail

action_152 _ = happyReduce_46

action_153 _ = happyReduce_3

action_154 _ = happyReduce_9

action_155 _ = happyReduce_67

action_156 (45) = happyShift action_47
action_156 (56) = happyShift action_48
action_156 (72) = happyShift action_49
action_156 (7) = happyGoto action_42
action_156 (8) = happyGoto action_43
action_156 (20) = happyGoto action_44
action_156 (21) = happyGoto action_170
action_156 (39) = happyGoto action_46
action_156 _ = happyFail

action_157 (57) = happyShift action_169
action_157 _ = happyFail

action_158 (57) = happyShift action_168
action_158 _ = happyFail

action_159 (57) = happyShift action_167
action_159 _ = happyFail

action_160 (57) = happyShift action_166
action_160 _ = happyFail

action_161 (56) = happyShift action_93
action_161 (72) = happyShift action_95
action_161 (6) = happyGoto action_82
action_161 (8) = happyGoto action_83
action_161 (33) = happyGoto action_165
action_161 (38) = happyGoto action_87
action_161 (39) = happyGoto action_88
action_161 _ = happyFail

action_162 _ = happyReduce_68

action_163 _ = happyReduce_21

action_164 _ = happyReduce_57

action_165 (50) = happyShift action_172
action_165 _ = happyFail

action_166 _ = happyReduce_80

action_167 _ = happyReduce_81

action_168 _ = happyReduce_79

action_169 _ = happyReduce_78

action_170 (57) = happyShift action_171
action_170 _ = happyFail

action_171 _ = happyReduce_47

action_172 (56) = happyShift action_112
action_172 (28) = happyGoto action_173
action_172 _ = happyFail

action_173 (58) = happyShift action_174
action_173 _ = happyFail

action_174 (55) = happyShift action_179
action_174 (56) = happyShift action_180
action_174 (72) = happyShift action_181
action_174 (6) = happyGoto action_175
action_174 (36) = happyGoto action_176
action_174 (37) = happyGoto action_177
action_174 (38) = happyGoto action_178
action_174 _ = happyFail

action_175 (56) = happyShift action_112
action_175 (66) = happyShift action_188
action_175 (68) = happyShift action_113
action_175 (26) = happyGoto action_109
action_175 (27) = happyGoto action_187
action_175 (28) = happyGoto action_111
action_175 _ = happyFail

action_176 (59) = happyShift action_186
action_176 _ = happyFail

action_177 (71) = happyShift action_185
action_177 _ = happyReduce_72

action_178 (66) = happyShift action_184
action_178 _ = happyFail

action_179 (66) = happyShift action_183
action_179 _ = happyFail

action_180 (74) = happyShift action_115
action_180 (75) = happyShift action_116
action_180 (76) = happyShift action_117
action_180 (77) = happyShift action_118
action_180 _ = happyFail

action_181 (62) = happyShift action_182
action_181 _ = happyFail

action_182 (73) = happyShift action_23
action_182 (9) = happyGoto action_153
action_182 _ = happyFail

action_183 (47) = happyShift action_89
action_183 (49) = happyShift action_90
action_183 (51) = happyShift action_91
action_183 (53) = happyShift action_92
action_183 (56) = happyShift action_93
action_183 (67) = happyShift action_94
action_183 (72) = happyShift action_95
action_183 (6) = happyGoto action_82
action_183 (8) = happyGoto action_83
action_183 (33) = happyGoto action_84
action_183 (34) = happyGoto action_85
action_183 (35) = happyGoto action_193
action_183 (38) = happyGoto action_87
action_183 (39) = happyGoto action_88
action_183 _ = happyFail

action_184 (47) = happyShift action_89
action_184 (49) = happyShift action_90
action_184 (51) = happyShift action_91
action_184 (53) = happyShift action_92
action_184 (56) = happyShift action_93
action_184 (67) = happyShift action_94
action_184 (72) = happyShift action_95
action_184 (6) = happyGoto action_82
action_184 (8) = happyGoto action_83
action_184 (33) = happyGoto action_84
action_184 (34) = happyGoto action_85
action_184 (35) = happyGoto action_192
action_184 (38) = happyGoto action_87
action_184 (39) = happyGoto action_88
action_184 _ = happyFail

action_185 (55) = happyShift action_179
action_185 (56) = happyShift action_180
action_185 (72) = happyShift action_181
action_185 (6) = happyGoto action_175
action_185 (36) = happyGoto action_191
action_185 (37) = happyGoto action_177
action_185 (38) = happyGoto action_178
action_185 _ = happyFail

action_186 _ = happyReduce_69

action_187 (66) = happyShift action_190
action_187 _ = happyFail

action_188 (47) = happyShift action_89
action_188 (49) = happyShift action_90
action_188 (51) = happyShift action_91
action_188 (53) = happyShift action_92
action_188 (56) = happyShift action_93
action_188 (67) = happyShift action_94
action_188 (72) = happyShift action_95
action_188 (6) = happyGoto action_82
action_188 (8) = happyGoto action_83
action_188 (33) = happyGoto action_84
action_188 (34) = happyGoto action_85
action_188 (35) = happyGoto action_189
action_188 (38) = happyGoto action_87
action_188 (39) = happyGoto action_88
action_188 _ = happyFail

action_189 _ = happyReduce_75

action_190 (47) = happyShift action_89
action_190 (49) = happyShift action_90
action_190 (51) = happyShift action_91
action_190 (53) = happyShift action_92
action_190 (56) = happyShift action_93
action_190 (67) = happyShift action_94
action_190 (72) = happyShift action_95
action_190 (6) = happyGoto action_82
action_190 (8) = happyGoto action_83
action_190 (33) = happyGoto action_84
action_190 (34) = happyGoto action_85
action_190 (35) = happyGoto action_194
action_190 (38) = happyGoto action_87
action_190 (39) = happyGoto action_88
action_190 _ = happyFail

action_191 _ = happyReduce_73

action_192 _ = happyReduce_76

action_193 _ = happyReduce_77

action_194 _ = happyReduce_74

happyReduce_1 = happyReduce 4 4 happyReduction_1
happyReduction_1 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (HsExtCore happy_var_2 [] []
	) `HappyStk` happyRest

happyReduce_2 = happySpecReduce_3  5 happyReduction_2
happyReduction_2 _
	_
	_
	 =  HappyAbsSyn5
		 (undefined
	)

happyReduce_3 = happySpecReduce_3  6 happyReduction_3
happyReduction_3 _
	_
	_
	 =  HappyAbsSyn6
		 (undefined
	)

happyReduce_4 = happySpecReduce_3  7 happyReduction_4
happyReduction_4 _
	_
	_
	 =  HappyAbsSyn6
		 (undefined
	)

happyReduce_5 = happySpecReduce_3  8 happyReduction_5
happyReduction_5 _
	_
	_
	 =  HappyAbsSyn6
		 (undefined
	)

happyReduce_6 = happySpecReduce_1  9 happyReduction_6
happyReduction_6 (HappyTerminal (TKcname happy_var_1))
	 =  HappyAbsSyn9
		 ([happy_var_1]
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  9 happyReduction_7
happyReduction_7 (HappyAbsSyn9  happy_var_3)
	_
	(HappyTerminal (TKcname happy_var_1))
	 =  HappyAbsSyn9
		 (happy_var_1:happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  10 happyReduction_8
happyReduction_8 (HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn9
		 ([happy_var_1]
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  10 happyReduction_9
happyReduction_9 (HappyAbsSyn9  happy_var_3)
	_
	(HappyTerminal (TKcname happy_var_1))
	 =  HappyAbsSyn9
		 (happy_var_1:happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_0  11 happyReduction_10
happyReduction_10  =  HappyAbsSyn11
		 ([]
	)

happyReduce_11 = happySpecReduce_2  11 happyReduction_11
happyReduction_11 (HappyAbsSyn11  happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1:happy_var_2
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happyReduce 8 12 happyReduction_12
happyReduction_12 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn30  happy_var_3) `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (DataDecl { tcdLName = noLoc (ifaceExtRdrName happy_var_2)
                   , tcdTyVars = mkHsQTvs (map toHsTvBndr happy_var_3)
                   , tcdDataDefn = HsDataDefn { dd_ND = DataType, dd_ctxt = noLoc [] 
     	                                      , dd_kindSig = Nothing
                                              , dd_cons = happy_var_6, dd_derivs = Nothing } }
	) `HappyStk` happyRest

happyReduce_13 = happyReduce 5 12 happyReduction_13
happyReduction_13 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_4) `HappyStk`
	(HappyAbsSyn30  happy_var_3) `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (let tc_rdr = ifaceExtRdrName happy_var_2 in
          DataDecl { tcdLName = noLoc tc_rdr
	           , tcdTyVars = mkHsQTvs (map toHsTvBndr happy_var_3)
                   , tcdDataDefn = HsDataDefn { dd_ND = NewType, dd_ctxt = noLoc []
		                              , dd_kindSig = Nothing
                                              , dd_cons = happy_var_4 (rdrNameOcc tc_rdr), dd_derivs = Nothing } }
	) `HappyStk` happyRest

happyReduce_14 = happySpecReduce_0  13 happyReduction_14
happyReduction_14  =  HappyAbsSyn13
		 ((\ tc_occ -> [])
	)

happyReduce_15 = happySpecReduce_2  13 happyReduction_15
happyReduction_15 (HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn13
		 ((\ tc_occ -> let { dc_name  = mkRdrUnqual (setOccNameSpace dataName tc_occ) ;
			                     con_info = PrefixCon [toHsType happy_var_2] }
			                in [noLoc $ mkSimpleConDecl (noLoc dc_name) []
					               (noLoc []) con_info])
	)
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_0  14 happyReduction_16
happyReduction_16  =  HappyAbsSyn14
		 ([]
	)

happyReduce_17 = happySpecReduce_1  14 happyReduction_17
happyReduction_17 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn14
		 ([happy_var_1]
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  14 happyReduction_18
happyReduction_18 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1:happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  15 happyReduction_19
happyReduction_19 (HappyAbsSyn17  happy_var_3)
	(HappyAbsSyn16  happy_var_2)
	(HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn15
		 (noLoc $ mkSimpleConDecl (noLoc (mkRdrUnqual happy_var_1)) happy_var_2 (noLoc []) (PrefixCon happy_var_3)
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_0  16 happyReduction_20
happyReduction_20  =  HappyAbsSyn16
		 ([]
	)

happyReduce_21 = happySpecReduce_3  16 happyReduction_21
happyReduction_21 (HappyAbsSyn16  happy_var_3)
	(HappyAbsSyn29  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (toHsTvBndr happy_var_2 : happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  17 happyReduction_22
happyReduction_22 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn17
		 (map toHsType happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_0  18 happyReduction_23
happyReduction_23  =  HappyAbsSyn18
		 ([]
	)

happyReduce_24 = happySpecReduce_2  18 happyReduction_24
happyReduction_24 (HappyAbsSyn18  happy_var_2)
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1:happy_var_2
	)
happyReduction_24 _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  19 happyReduction_25
happyReduction_25 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn19
		 (IfaceTyVar happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  19 happyReduction_26
happyReduction_26 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn19
		 (IfaceTyConApp (IfaceTc happy_var_1) []
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  19 happyReduction_27
happyReduction_27 _
	(HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn19
		 (happy_var_2
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_2  20 happyReduction_28
happyReduction_28 (HappyAbsSyn18  happy_var_2)
	(HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn19
		 (foldl IfaceAppTy (IfaceTyVar happy_var_1) happy_var_2
	)
happyReduction_28 _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_2  20 happyReduction_29
happyReduction_29 _
	_
	 =  HappyAbsSyn19
		 (undefined
	)

happyReduce_30 = happySpecReduce_2  20 happyReduction_30
happyReduction_30 (HappyAbsSyn18  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn19
		 (IfaceTyConApp (IfaceTc happy_var_1) happy_var_2
	)
happyReduction_30 _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  20 happyReduction_31
happyReduction_31 _
	(HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn19
		 (happy_var_2
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  21 happyReduction_32
happyReduction_32 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  21 happyReduction_33
happyReduction_33 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (IfaceFunTy happy_var_1 happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happyReduce 4 21 happyReduction_34
happyReduction_34 ((HappyAbsSyn19  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn30  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (foldr IfaceForAllTy happy_var_4 happy_var_2
	) `HappyStk` happyRest

happyReduce_35 = happySpecReduce_0  22 happyReduction_35
happyReduction_35  =  HappyAbsSyn22
		 ([]
	)

happyReduce_36 = happySpecReduce_3  22 happyReduction_36
happyReduction_36 (HappyAbsSyn22  happy_var_3)
	_
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1 : happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happyReduce 4 23 happyReduction_37
happyReduction_37 (_ `HappyStk`
	(HappyAbsSyn24  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn23
		 (IfaceRec happy_var_3
	) `HappyStk` happyRest

happyReduce_38 = happySpecReduce_1  23 happyReduction_38
happyReduction_38 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn23
		 (let (b,r) = happy_var_1
				  in IfaceNonRec b r
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_1  24 happyReduction_39
happyReduction_39 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn24
		 ([happy_var_1]
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_3  24 happyReduction_40
happyReduction_40 (HappyAbsSyn24  happy_var_3)
	_
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1:happy_var_3
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happyReduce 5 25 happyReduction_41
happyReduction_41 ((HappyAbsSyn33  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn39  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 ((IfLetBndr happy_var_1 happy_var_3 NoInfo, happy_var_5)
	) `HappyStk` happyRest

happyReduce_42 = happySpecReduce_2  25 happyReduction_42
happyReduction_42 (HappyAbsSyn25  happy_var_2)
	_
	 =  HappyAbsSyn25
		 (happy_var_2
	)
happyReduction_42 _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_2  26 happyReduction_43
happyReduction_43 (HappyAbsSyn29  happy_var_2)
	_
	 =  HappyAbsSyn26
		 (IfaceTvBndr happy_var_2
	)
happyReduction_43 _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  26 happyReduction_44
happyReduction_44 (HappyAbsSyn28  happy_var_1)
	 =  HappyAbsSyn26
		 (IfaceIdBndr happy_var_1
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  27 happyReduction_45
happyReduction_45 (HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn27
		 ([happy_var_1]
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_2  27 happyReduction_46
happyReduction_46 (HappyAbsSyn27  happy_var_2)
	(HappyAbsSyn26  happy_var_1)
	 =  HappyAbsSyn27
		 (happy_var_1:happy_var_2
	)
happyReduction_46 _ _  = notHappyAtAll 

happyReduce_47 = happyReduce 5 28 happyReduction_47
happyReduction_47 (_ `HappyStk`
	(HappyAbsSyn19  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn39  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn28
		 ((happy_var_2,happy_var_4)
	) `HappyStk` happyRest

happyReduce_48 = happySpecReduce_1  29 happyReduction_48
happyReduction_48 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn29
		 ((happy_var_1, ifaceLiftedTypeKind)
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happyReduce 5 29 happyReduction_49
happyReduction_49 (_ `HappyStk`
	(HappyAbsSyn31  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn39  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn29
		 ((happy_var_2, happy_var_4)
	) `HappyStk` happyRest

happyReduce_50 = happySpecReduce_0  30 happyReduction_50
happyReduction_50  =  HappyAbsSyn30
		 ([]
	)

happyReduce_51 = happySpecReduce_2  30 happyReduction_51
happyReduction_51 (HappyAbsSyn30  happy_var_2)
	(HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn30
		 (happy_var_1:happy_var_2
	)
happyReduction_51 _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_1  31 happyReduction_52
happyReduction_52 _
	 =  HappyAbsSyn31
		 (ifaceLiftedTypeKind
	)

happyReduce_53 = happySpecReduce_1  31 happyReduction_53
happyReduction_53 _
	 =  HappyAbsSyn31
		 (ifaceUnliftedTypeKind
	)

happyReduce_54 = happySpecReduce_1  31 happyReduction_54
happyReduction_54 _
	 =  HappyAbsSyn31
		 (ifaceOpenTypeKind
	)

happyReduce_55 = happySpecReduce_3  31 happyReduction_55
happyReduction_55 _
	(HappyAbsSyn31  happy_var_2)
	_
	 =  HappyAbsSyn31
		 (happy_var_2
	)
happyReduction_55 _ _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  32 happyReduction_56
happyReduction_56 (HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn31
		 (happy_var_1
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_3  32 happyReduction_57
happyReduction_57 (HappyAbsSyn31  happy_var_3)
	_
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn31
		 (ifaceArrow happy_var_1 happy_var_3
	)
happyReduction_57 _ _ _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_1  33 happyReduction_58
happyReduction_58 (HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn33
		 (IfaceLcl happy_var_1
	)
happyReduction_58 _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_1  33 happyReduction_59
happyReduction_59 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn33
		 (IfaceExt happy_var_1
	)
happyReduction_59 _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_1  33 happyReduction_60
happyReduction_60 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn33
		 (IfaceExt happy_var_1
	)
happyReduction_60 _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_1  33 happyReduction_61
happyReduction_61 (HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn33
		 (IfaceLit happy_var_1
	)
happyReduction_61 _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_3  33 happyReduction_62
happyReduction_62 _
	(HappyAbsSyn33  happy_var_2)
	_
	 =  HappyAbsSyn33
		 (happy_var_2
	)
happyReduction_62 _ _ _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_2  34 happyReduction_63
happyReduction_63 (HappyAbsSyn33  happy_var_2)
	(HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn33
		 (IfaceApp happy_var_1 happy_var_2
	)
happyReduction_63 _ _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_3  34 happyReduction_64
happyReduction_64 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn33
		 (IfaceApp happy_var_1 (IfaceType happy_var_3)
	)
happyReduction_64 _ _ _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_1  34 happyReduction_65
happyReduction_65 (HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn33
		 (happy_var_1
	)
happyReduction_65 _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_1  35 happyReduction_66
happyReduction_66 (HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn33
		 (happy_var_1
	)
happyReduction_66 _  = notHappyAtAll 

happyReduce_67 = happyReduce 4 35 happyReduction_67
happyReduction_67 ((HappyAbsSyn33  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn33
		 (foldr IfaceLam happy_var_4 happy_var_2
	) `HappyStk` happyRest

happyReduce_68 = happyReduce 4 35 happyReduction_68
happyReduction_68 ((HappyAbsSyn33  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn33
		 (IfaceLet happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_69 = happyReduce 10 35 happyReduction_69
happyReduction_69 (_ `HappyStk`
	(HappyAbsSyn36  happy_var_9) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn28  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn33  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn33
		 (IfaceCase happy_var_5 (fst happy_var_7) happy_var_9
	) `HappyStk` happyRest

happyReduce_70 = happySpecReduce_3  35 happyReduction_70
happyReduction_70 (HappyAbsSyn19  happy_var_3)
	(HappyAbsSyn33  happy_var_2)
	_
	 =  HappyAbsSyn33
		 (IfaceCast happy_var_2 happy_var_3
	)
happyReduction_70 _ _ _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_3  35 happyReduction_71
happyReduction_71 (HappyAbsSyn19  happy_var_3)
	(HappyTerminal (TKstring happy_var_2))
	_
	 =  HappyAbsSyn33
		 (IfaceFCall (ForeignCall.CCall 
                                                    (CCallSpec (StaticTarget (mkFastString happy_var_2) Nothing True) 
                                                               CCallConv PlaySafe)) 
                                                 happy_var_3
	)
happyReduction_71 _ _ _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_1  36 happyReduction_72
happyReduction_72 (HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn36
		 ([happy_var_1]
	)
happyReduction_72 _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_3  36 happyReduction_73
happyReduction_73 (HappyAbsSyn36  happy_var_3)
	_
	(HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn36
		 (happy_var_1:happy_var_3
	)
happyReduction_73 _ _ _  = notHappyAtAll 

happyReduce_74 = happyReduce 4 37 happyReduction_74
happyReduction_74 ((HappyAbsSyn33  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_2) `HappyStk`
	(HappyAbsSyn6  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn37
		 ((IfaceDataAlt happy_var_1, map ifaceBndrName happy_var_2, happy_var_4)
	) `HappyStk` happyRest

happyReduce_75 = happySpecReduce_3  37 happyReduction_75
happyReduction_75 (HappyAbsSyn33  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn37
		 ((IfaceDataAlt happy_var_1, [], happy_var_3)
	)
happyReduction_75 _ _ _  = notHappyAtAll 

happyReduce_76 = happySpecReduce_3  37 happyReduction_76
happyReduction_76 (HappyAbsSyn33  happy_var_3)
	_
	(HappyAbsSyn38  happy_var_1)
	 =  HappyAbsSyn37
		 ((IfaceLitAlt happy_var_1, [], happy_var_3)
	)
happyReduction_76 _ _ _  = notHappyAtAll 

happyReduce_77 = happySpecReduce_3  37 happyReduction_77
happyReduction_77 (HappyAbsSyn33  happy_var_3)
	_
	_
	 =  HappyAbsSyn37
		 ((IfaceDefault, [], happy_var_3)
	)
happyReduction_77 _ _ _  = notHappyAtAll 

happyReduce_78 = happyReduce 5 38 happyReduction_78
happyReduction_78 (_ `HappyStk`
	(HappyAbsSyn19  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TKinteger happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn38
		 (convIntLit happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_79 = happyReduce 5 38 happyReduction_79
happyReduction_79 (_ `HappyStk`
	(HappyAbsSyn19  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TKrational happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn38
		 (convRatLit happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_80 = happyReduce 5 38 happyReduction_80
happyReduction_80 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TKchar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn38
		 (MachChar happy_var_2
	) `HappyStk` happyRest

happyReduce_81 = happyReduce 5 38 happyReduction_81
happyReduction_81 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TKstring happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn38
		 (MachStr (fastStringToByteString (mkFastString happy_var_2))
	) `HappyStk` happyRest

happyReduce_82 = happySpecReduce_1  39 happyReduction_82
happyReduction_82 (HappyTerminal (TKname happy_var_1))
	 =  HappyAbsSyn39
		 (mkFastString happy_var_1
	)
happyReduction_82 _  = notHappyAtAll 

happyReduce_83 = happySpecReduce_1  40 happyReduction_83
happyReduction_83 (HappyTerminal (TKname happy_var_1))
	 =  HappyAbsSyn40
		 (happy_var_1
	)
happyReduction_83 _  = notHappyAtAll 

happyReduce_84 = happySpecReduce_1  41 happyReduction_84
happyReduction_84 (HappyTerminal (TKcname happy_var_1))
	 =  HappyAbsSyn41
		 (mkOccName dataName happy_var_1
	)
happyReduction_84 _  = notHappyAtAll 

happyNewToken action sts stk
	= lexer(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	TKEOF -> action 78 78 tk (HappyState action) sts stk;
	TKmodule -> cont 42;
	TKdata -> cont 43;
	TKnewtype -> cont 44;
	TKforall -> cont 45;
	TKrec -> cont 46;
	TKlet -> cont 47;
	TKin -> cont 48;
	TKcase -> cont 49;
	TKof -> cont 50;
	TKcast -> cont 51;
	TKnote -> cont 52;
	TKexternal -> cont 53;
	TKlocal -> cont 54;
	TKwild -> cont 55;
	TKoparen -> cont 56;
	TKcparen -> cont 57;
	TKobrace -> cont 58;
	TKcbrace -> cont 59;
	TKhash -> cont 60;
	TKeq -> cont 61;
	TKcolon -> cont 62;
	TKcoloncolon -> cont 63;
	TKcoloneqcolon -> cont 64;
	TKstar -> cont 65;
	TKrarrow -> cont 66;
	TKlambda -> cont 67;
	TKat -> cont 68;
	TKdot -> cont 69;
	TKquestion -> cont 70;
	TKsemicolon -> cont 71;
	TKname happy_dollar_dollar -> cont 72;
	TKcname happy_dollar_dollar -> cont 73;
	TKinteger happy_dollar_dollar -> cont 74;
	TKrational happy_dollar_dollar -> cont 75;
	TKstring happy_dollar_dollar -> cont 76;
	TKchar happy_dollar_dollar -> cont 77;
	_ -> happyError' tk
	})

happyError_ 78 tk = happyError' tk
happyError_ _ tk = happyError' tk

happyThen :: () => P a -> (a -> P b) -> P b
happyThen = (thenP)
happyReturn :: () => a -> P a
happyReturn = (returnP)
happyThen1 = happyThen
happyReturn1 :: () => a -> P a
happyReturn1 = happyReturn
happyError' :: () => (Token) -> P a
happyError' tk = (\token -> happyError) tk

parseCore = happySomeParser where
  happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


ifaceKind kc = IfaceTyConApp kc []

ifaceBndrName (IfaceIdBndr (n,_)) = n
ifaceBndrName (IfaceTvBndr (n,_)) = n

convIntLit :: Integer -> IfaceType -> Literal
convIntLit i (IfaceTyConApp tc [])
  | tc `eqTc` intPrimTyCon  = MachInt  i  
  | tc `eqTc` wordPrimTyCon = MachWord i
  | tc `eqTc` charPrimTyCon = MachChar (chr (fromInteger i))
  | tc `eqTc` addrPrimTyCon && i == 0 = MachNullAddr
convIntLit i aty
  = pprPanic "Unknown integer literal type" (ppr aty)

convRatLit :: Rational -> IfaceType -> Literal
convRatLit r (IfaceTyConApp tc [])
  | tc `eqTc` floatPrimTyCon  = MachFloat  r
  | tc `eqTc` doublePrimTyCon = MachDouble r
convRatLit i aty
  = pprPanic "Unknown rational literal type" (ppr aty)

eqTc :: IfaceTyCon -> TyCon -> Bool   -- Ugh!
eqTc (IfaceTc name) tycon = name == tyConName tycon

-- Tiresomely, we have to generate both HsTypes (in type/class decls) 
-- and IfaceTypes (in Core expressions).  So we parse them as IfaceTypes,
-- and convert to HsTypes here.  But the IfaceTypes we can see here
-- are very limited (see the productions for 'ty'), so the translation
-- isn't hard
toHsType :: IfaceType -> LHsType RdrName
toHsType (IfaceTyVar v)        		 = noLoc $ HsTyVar (mkRdrUnqual (mkTyVarOccFS v))
toHsType (IfaceAppTy t1 t2)    		 = noLoc $ HsAppTy (toHsType t1) (toHsType t2)
toHsType (IfaceFunTy t1 t2)    		 = noLoc $ HsFunTy (toHsType t1) (toHsType t2)
toHsType (IfaceTyConApp (IfaceTc tc) ts) = foldl mkHsAppTy (noLoc $ HsTyVar (ifaceExtRdrName tc)) (map toHsType ts) 
toHsType (IfaceForAllTy tv t)            = add_forall (toHsTvBndr tv) (toHsType t)

-- Only a limited form of kind will be encountered... hopefully
toHsKind :: IfaceKind -> LHsKind RdrName
-- IA0_NOTE: Shouldn't we add kind variables?
toHsKind (IfaceFunTy ifK1 ifK2)  = noLoc $ HsFunTy (toHsKind ifK1) (toHsKind ifK2)
toHsKind (IfaceTyConApp ifKc []) = noLoc $ HsTyVar (nameRdrName (tyConName (toKindTc ifKc)))
toHsKind other                   = pprPanic "toHsKind" (ppr other)

toKindTc :: IfaceTyCon -> TyCon
toKindTc (IfaceTc n) | Just (ATyCon tc) <- wiredInNameTyThing_maybe n = tc
toKindTc other = pprPanic "toKindTc" (ppr other)

ifaceTcType ifTc = IfaceTyConApp ifTc []

ifaceLiftedTypeKind   = ifaceTcType (IfaceTc liftedTypeKindTyConName)
ifaceOpenTypeKind     = ifaceTcType (IfaceTc openTypeKindTyConName)
ifaceUnliftedTypeKind = ifaceTcType (IfaceTc unliftedTypeKindTyConName)

ifaceArrow ifT1 ifT2 = IfaceFunTy ifT1 ifT2

toHsTvBndr :: IfaceTvBndr -> LHsTyVarBndr RdrName
toHsTvBndr (tv,k) = noLoc $ KindedTyVar (mkRdrUnqual (mkTyVarOccFS tv)) bsig
                  where
                    bsig = toHsKind k

ifaceExtRdrName :: Name -> RdrName
ifaceExtRdrName name = mkOrig (nameModule name) (nameOccName name)
ifaceExtRdrName other = pprPanic "ParserCore.ifaceExtRdrName" (ppr other)

add_forall tv (L _ (HsForAllTy exp tvs cxt t))
  = noLoc $ HsForAllTy exp (mkHsQTvs (tv : hsQTvBndrs tvs)) cxt t
add_forall tv t
  = noLoc $ HsForAllTy Explicit (mkHsQTvs [tv]) (noLoc []) t
  
happyError :: P a 
happyError s l = failP (show l ++ ": Parse error\n") (take 100 s) l
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
