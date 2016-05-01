module BP.Env where

import BP.Ast
import BP.Type
import State
import qualified Data.Map as M

type Env = M.Map EName Type

tvarA :: Type
tvarA = TypeVariable 0 (createState Nothing) (createState Nothing)

tvarB :: Type
tvarB = TypeVariable 1 (createState Nothing) (createState Nothing)

tvarC :: Type
tvarC = TypeVariable 2 (createState Nothing) (createState Nothing)

pairT :: Type
pairT = TypeOperator "*" [tvarA, tvarB]

assumptions :: Env
assumptions = M.fromList
  [("pair", functionT tvarA $ functionT tvarB pairT),
   ("true", boolT),
   ("cond", functionT boolT $ functionT tvarA $ functionT tvarA tvarA),
   ("zero?", functionT intT boolT),
   ("pred", functionT intT intT),
   ("times", functionT intT $ functionT intT intT)]

pairE :: Term
pairE = Apply (Apply (Ident "pair") $ Apply (Ident "f") $ Ident "4") $ Apply (Ident "f") $ Ident "true"