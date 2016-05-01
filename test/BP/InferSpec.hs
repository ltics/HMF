module BP.InferSpec where

import BP.Ast
import BP.Type
import BP.Infer
import BP.Env
import State (resetId, resetUniqueName)
import qualified Text.PrettyPrint as PP
import qualified Data.Set as S
import Test.Hspec

runInferSpecCase :: Term -> String -> IO ()
runInferSpecCase expr expect = do
    assumps <- assumptions
    t <- analyze expr assumps S.empty
    resetId
    resetUniqueName
    (PP.text . show $ t) `shouldBe` PP.text expect

failInferSpecCase :: Term -> String -> IO ()
failInferSpecCase expr error = do
    assumps <- assumptions
    analyze expr assumps S.empty `shouldThrow` errorCall error
    resetId
    resetUniqueName

spec :: Spec
spec = describe "inference test" $
        it "should inference type of given term" $ do
          -- recursive type
          runInferSpecCase (LetRec "factorial" (Lambda "n" $ Apply (Apply (Apply (Ident "cond") $ Apply (Ident "zero?") $ Ident "n") $ Ident "1") (Apply (Apply (Ident "times") $ Ident "n") $ Apply (Ident "factorial") $ Apply (Ident "pred") $ Ident "n")) $ Apply (Ident "factorial") $ Ident "5") "int"
          failInferSpecCase (Lambda "x" $ Apply (Apply (Ident "pair") $ Apply (Ident "x") $ Ident "3") $ Apply (Ident "x") $ Ident "true") "Type mismatch bool ≠ int"
          failInferSpecCase (Apply (Apply (Ident "pair") $ Apply (Ident "g") $ Ident "3") $ Apply (Ident "g") $ Ident "true") "Undefined symbol g"
          runInferSpecCase (Lambda "x" $ Ident "x") "(α → α)"
          runInferSpecCase (Apply (Ident "zero?") $ Ident "3") "bool"
          runInferSpecCase (Let "f" (Lambda "x" $ Ident "x") pairE) "(int * bool)"
          failInferSpecCase (Lambda "f" $ Apply (Ident "f") $ Ident "f") "Recusive unification"
          runInferSpecCase (Let "g" (Lambda "f" $ Ident"5") $ Apply (Ident "g") $ Ident "g") "int"
          -- bound variable of lambda expression is non-generic and bound variable of let expression is generic
          runInferSpecCase (Lambda "g" $ Let "f" (Lambda "x" $ Ident "g") $ Apply (Apply (Ident "pair") $ Apply (Ident "f") $ Ident "3") $ Apply (Ident "f") $ Ident "true") "(α → (α * α))"
          -- function composition maybe the inferred types should be normalized
          runInferSpecCase (Lambda "f" $ Lambda "g" $ Lambda "arg" $ Apply (Ident "f") $ Apply (Ident "g") $ Ident "arg") "((α → β) → ((γ → α) → (γ → β)))"
          runInferSpecCase pairE "(int * bool)"
          runInferSpecCase (Apply (Ident "f") $ Ident "3") "int"
          runInferSpecCase (Apply (Ident "f") $ Ident "true") "bool"
          runInferSpecCase (Let "f" (Lambda "x" $ Ident "x") $ Apply (Ident "f") $ Ident "3") "int"
