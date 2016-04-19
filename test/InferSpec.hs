module InferSpec where

import Ast
import Type
import Infer
import State
import Data.IORef
import qualified Data.Map as M
import qualified Text.PrettyPrint as PP
import Test.Hspec

runInferSpecCase expr expect = do
    t <- infer assumptions 0 expr
    gt <- generalize (-1) t
    resetId
    (PP.text . show $ gt) `shouldBe` PP.text expect

spec :: Spec
spec = do
    describe "create var" $ do
      it "should create vars" $ do
        params <- mapM (\_ -> newVar 0) [1..3]
        resetId
        (PP.text . show $ params) `shouldBe` PP.text "[_0,_1,_2]"
    describe "unification test" $ do
      it "unify arrow type" $ do
        a <- newVar 0
        b <- newVar 0
        c <- newVar 0
        resetId
        unify (TArrow [TConst "int", TConst "bool"] $ TConst "int") (TArrow [a, b] c)
        (PP.text . show $ a) `shouldBe` PP.text "int"
        (PP.text . show $ b) `shouldBe` PP.text "bool"
        (PP.text . show $ c) `shouldBe` PP.text "int"
      it "unify const type" $ do
        a <- newVar 0
        resetId
        unify (TConst "int") a
        (PP.text . show $ a) `shouldBe` PP.text "int"
    describe "inference test" $ do
      it "should infer most general or principal types for given expression" $ do
        runInferSpecCase (EVar "id") "∀a. a → a"
        runInferSpecCase (EVar "one") "int"
        (infer assumptions 0 $ EVar "x") `shouldThrow` errorCall "variable x not found"
        (infer assumptions 0 $ ELet "x" (EVar "x") (EVar "x")) `shouldThrow` errorCall "variable x not found"
        runInferSpecCase (ELet "x" (EVar "id") $ EVar "x") "∀a. a → a"
        runInferSpecCase (ELet "x" (EFun ["y"] $ EVar "y") $ EVar "x") "∀a. a → a"
        runInferSpecCase (EFun ["x"] $ EVar "x") "∀a. a → a"
        runInferSpecCase (EVar "pair") "∀a,b. (a, b) → pair[a, b]"
        runInferSpecCase (EFun ["x"] $ ELet "y" (EFun ["z"] $ EVar "z") $ EVar "y") "∀a,b. a → b → b"
        runInferSpecCase (EFun ["x"] $ ELet "y" (EFun ["z"] $ EVar "x") $ EVar "y") "∀a,b. a → b → a"
        runInferSpecCase (ELet "f" (EFun ["x"] $ EVar "x") $ ELet "id" (EFun ["y"] $ EVar "y") $ ECall (EVar "eq") [EVar "f", EVar "id"]) "bool"
        runInferSpecCase (ELet "f" (EFun ["x"] $ EVar "x") $ ELet "id" (EFun ["y"] $ EVar "y") $ ECall (ECall (EVar "eq-curry") [EVar "f"]) [EVar "id"]) "bool"
        runInferSpecCase (ELet "f" (EFun ["x"] $ EVar "x") $ ECall (EVar "eq") [EVar "f", EVar "succ"]) "bool"
        runInferSpecCase (ELet "f" (EFun ["x"] $ EVar "x") $ ECall (ECall (EVar "eq-curry") [EVar "f"]) [EVar "succ"]) "bool"
        -- let polymorphism
        runInferSpecCase (ELet "f" (EFun ["x"] $ EVar "x") $ ECall (EVar "pair") [ECall (EVar "f") [EVar "one"], ECall (EVar "f") [EVar "true"]]) "pair[int, bool]"
