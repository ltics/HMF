{-# OPTIONS_GHC -Wall #-}

module FCP.Env where

import FCP.Ast
import FCP.Type
import State
import qualified Data.Map as M

type Env = M.Map EName T

tcInt :: T
tcInt = TConst "int"

tcBool :: T
tcBool = TConst "bool"

tcList :: T
tcList = TConst "list"

tcPair :: T
tcPair = TConst "pair"

tvarA :: T
tvarA = TVar (createState (Bound 0))

tvarB :: T
tvarB = TVar (createState (Bound 1))

polyA :: T
polyA = TForall [0] $ TArrow [tvarA] tvarA

tvarListA :: T
tvarListA = TApp tcList [tvarA]

tvarListB :: T
tvarListB = TApp tcList [tvarB]

polyListA :: T
polyListA = TApp tcList [polyA]

polyPair :: T
polyPair = TApp tcPair [tvarA, tvarB]

assumptions :: Env
assumptions = M.fromList
    [("head", TForall [0] $ TArrow [tvarListA] tvarA),
     ("tail", TForall [0] $ TArrow [tvarListA] tvarListA),
     ("nil", TForall [0] $ tvarListA),
     ("cons", TForall [0] $ TArrow [tvarA, tvarListA] tvarListA),
     ("cons-curry", TForall [0] $ TArrow [tvarA] $ TArrow [tvarListA] tvarListA),
     ("map", TForall [0, 1] $ TArrow [TArrow [tvarA] tvarB, tvarListA] tvarListB),
     ("map-curry", TForall [0, 1] $ TArrow [TArrow [tvarA] tvarB] $ TArrow [tvarListA] tvarListB),
     ("single", TForall [0] $ TArrow [tvarA] tvarListA),
     ("length", TForall [0] $ TArrow [tvarListA] tcInt),
     ("zero", tcInt),
     ("one", tcInt),
     ("succ", TArrow [tcInt] tcInt),
     ("plus", TArrow [tcInt, tcInt] tcInt),
     ("true", tcBool),
     ("false", tcBool),
     ("not", TArrow [tcBool] tcBool),
     ("eq", TForall [0] $ TArrow [tvarA, tvarA] tcBool),
     ("eq-curry", TForall [0] $ TArrow [tvarA] $ TArrow [tvarA] tcBool),
     ("pair", TForall [0, 1] $ TArrow [tvarA, tvarB] polyPair),
     ("pair-curry", TForall [0, 1] $ TArrow [tvarA] $ TArrow [tvarB] polyPair),
     ("first", TForall [0] $ TArrow [polyPair] tvarA),
     ("second", TForall [0] $ TArrow [polyPair] tvarB),
     ("id", polyA),
     ("ids", polyListA),
     ("id->id", TArrow [polyA] polyA),
     ("almost-id-id", TForall [0] $ TArrow [polyA] $ TArrow [tvarA] tvarA),
     ("id-ids", TArrow [polyListA] polyListA),
     ("id-magic", TArrow [TForall [0, 1] $ TArrow [tvarA] tvarB] $ TForall [0, 1] $ TArrow [tvarA] tvarB),
     ("id-succ", TArrow [TArrow [tcInt] tcInt] $ TArrow [tcInt] tcInt),
     ("const", TForall [0, 1] $ TArrow [tvarA] $ TArrow [tvarB] tvarA),
     ("apply", TForall [0, 1] $ TArrow [TArrow [tvarA] tvarB, tvarA] tvarB),
     ("apply-curry", TForall [0, 1] $ TArrow [TArrow [tvarA] tvarB] $ TArrow [tvarA] tvarB),
     ("rev-apply", TForall [0, 1] $ TArrow [tvarA, TArrow [tvarA] tvarB] tvarB),
     ("rev-apply-curry", TForall [0, 1] $ TArrow [tvarA] $ TArrow [TArrow [tvarA] tvarB] tvarB),
     ("choose", TForall [0] $ TArrow [tvarA, tvarA] tvarA),
     ("choose-curry", TForall [0] $ TArrow [tvarA] $ TArrow [tvarA] tvarA),
     ("magic", TForall [0, 1] $ TArrow [tvarA] tvarB),
     ("any", TForall [0] tvarA),
     ("poly", TArrow [polyA] $ TApp tcPair [tcInt, tcBool]),
     ("special", TArrow [TArrow [polyA] polyA] polyA)]
