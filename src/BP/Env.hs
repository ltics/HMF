module BP.Env where

import BP.Ast
import BP.Type
import State
import System.IO.Unsafe(unsafePerformIO)
import qualified Data.Map as M

type Env = M.Map EName Type

assumptions :: Infer Env
assumptions = do
  tvarA <- makeVariable
  tvarB <- makeVariable
  let pairT = TypeOperator "*" [tvarA, tvarB]
  return $ M.fromList [("pair", functionT tvarA $ functionT tvarB pairT),
                       ("true", boolT),
                       ("f", functionT tvarA tvarA),
                       ("cond", functionT boolT $ functionT tvarA $ functionT tvarA tvarA),
                       ("zero?", functionT intT boolT),
                       ("pred", functionT intT intT),
                       ("times", functionT intT $ functionT intT intT)]

pairE :: Term
pairE = Apply (Apply (Ident "pair") $ Apply (Ident "f") $ Ident "4") $ Apply (Ident "f") $ Ident "true"
