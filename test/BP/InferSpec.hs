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
          runInferSpecCase (LetRec "factorial" (Lambda "n" $ Apply (Apply (Apply (Ident "cond") $ Apply (Ident "zero?") $ Ident "n") $ Ident "1") (Apply (Apply (Ident "times") $ Ident "n") $ Apply (Ident "factorial") $ Apply (Ident "pred") $ Ident "n")) $ Apply (Ident "factorial") $ Ident "5") "int"
          failInferSpecCase (Apply (Apply (Ident "pair") $ Apply (Ident "g") $ Ident "4") $ Apply (Ident "g") $ Ident "true") "Undefined symbol g"
          runInferSpecCase (Lambda "x" $ Ident "x") "(α → α)"
          runInferSpecCase (Apply (Ident "zero?") $ Ident "3") "bool"
          runInferSpecCase (Let "f" (Lambda "x" $ Ident "x") pairE) "(int * bool)"
          runInferSpecCase pairE "(int * bool)"
          runInferSpecCase (Apply (Ident "f") $ Ident "3") "int"
          runInferSpecCase (Apply (Ident "f") $ Ident "true") "bool"
          runInferSpecCase (Let "f" (Lambda "x" $ Ident "x") $ Apply (Ident "f") $ Ident "3") "int"
