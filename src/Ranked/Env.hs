{-# OPTIONS_GHC -Wall #-}

module Ranked.Env where

import Ranked.Ast
import Ranked.Type
import State
import qualified Data.Map as M

type Env = M.Map Ranked.Ast.Name T

tcInt :: T
tcInt = TConst "int"

tcBool :: T
tcBool = TConst "bool"

tcList :: T
tcList = TConst "list"

tcPair :: T
tcPair = TConst "pair"

tvarA :: T
tvarA = TVar (createState (Generic 0))

tvarB :: T
tvarB = TVar (createState (Generic 1))

tvarListA :: T
tvarListA = TApp tcList [tvarA]

tvarListB :: T
tvarListB = TApp tcList [tvarB]

polyPair :: T
polyPair = TApp tcPair [tvarA, tvarB]

assumptions :: Env
assumptions = M.fromList
    [("head", TArrow [tvarListA] tvarA),
     ("tail", TArrow [tvarListA] tvarListA),
     ("nil", tvarListA),
     ("cons", TArrow [tvarA, tvarListA] tvarListA),
     ("cons-curry", TArrow [tvarA] $ TArrow [tvarListA] tvarListA),
     ("map", TArrow [TArrow [tvarA] tvarB, tvarListA] tvarListB),
     ("map-curry", TArrow [TArrow [tvarA] tvarB] $ TArrow [tvarListA] tvarListB),
     ("single", TArrow [tvarA] tvarListA),
     ("zero", tcInt),
     ("one", tcInt),
     ("succ", TArrow [tcInt] tcInt),
     ("plus", TArrow [tcInt, tcInt] tcInt),
     ("true", tcBool),
     ("false", tcBool),
     ("not", TArrow [tcBool] tcBool),
     ("eq", TArrow [tvarA, tvarA] tcBool),
     ("eq-curry", TArrow [tvarA] $ TArrow [tvarA] tcBool),
     ("pair", TArrow [tvarA, tvarB] polyPair),
     ("pair-curry", TArrow [tvarA] $ TArrow [tvarB] polyPair),
     ("first", TArrow [polyPair] tvarA),
     ("second", TArrow [polyPair] tvarB),
     ("id", TArrow [tvarA] tvarA),
     ("const", TArrow [tvarA] $ TArrow [tvarB] tvarA),
     ("apply", TArrow [TArrow [tvarA] tvarB, tvarA] tvarB),
     ("apply-curry", TArrow [TArrow [tvarA] tvarB] $ TArrow [tvarA] tvarB),
     ("choose", TArrow [tvarA, tvarA] tvarA),
     ("choose-curry", TArrow [tvarA] $ TArrow [tvarA] tvarA)]