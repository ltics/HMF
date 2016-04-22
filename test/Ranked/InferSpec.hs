module Ranked.InferSpec where

import Ranked.Ast
import Ranked.Type
import Ranked.Infer
import Ranked.Env
import State (resetId)
import qualified Text.PrettyPrint as PP
import Test.Hspec

runInferSpecCase :: Expr -> String -> IO ()
runInferSpecCase expr expect = do
    t <- infer assumptions 0 expr
    gt <- generalize (-1) t
    resetId
    (PP.text . show $ gt) `shouldBe` PP.text expect

failInferSpecCase :: Expr -> String -> IO ()
failInferSpecCase expr error = do
    infer assumptions 0 expr `shouldThrow` errorCall error
    resetId

spec :: Spec
spec = do
    describe "create var" $
      it "should create vars" $ do
        params <- mapM (\ _ -> newVar 0) [1..3]
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
    describe "inference test" $
      it "should infer most general or principal types for given expression" $ do
        runInferSpecCase (EVar "id") "∀a. a → a"
        runInferSpecCase (EVar "one") "int"
        failInferSpecCase (EVar "x") "variable x not found"
        failInferSpecCase (ELet "x" (EVar "x") (EVar "x")) "variable x not found"
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
        failInferSpecCase (EFun ["f"] $ ECall (EVar "pair") [ECall (EVar "f") [EVar "one"], ECall (EVar "f") [EVar "true"]]) "cannot unify types int and bool"
        runInferSpecCase (ELet "f" (EFun ["x", "y"] $ ELet "a" (ECall (EVar "eq") [EVar "x", EVar "y"]) $ ECall (EVar "eq") [EVar "x", EVar "y"]) $ EVar "f") "∀a. (a, a) → bool"
        runInferSpecCase (ELet "f" (EFun ["x", "y"] $ ELet "a" (ECall (ECall (EVar "eq-curry") [EVar "x"]) [EVar "y"]) $ ECall (ECall (EVar "eq-curry") [EVar "x"]) [EVar "y"]) $ EVar "f") "∀a. (a, a) → bool"
        runInferSpecCase (ECall (EVar "id") [EVar "id"]) "∀a. a → a"
        runInferSpecCase (ECall (EVar "choose") [EFun ["x", "y"] (EVar "x"), EFun ["x", "y"] (EVar "y")]) "∀a. (a, a) → a"
        runInferSpecCase (ECall (ECall (EVar "choose-curry") [EFun ["x", "y"] (EVar "x")]) [EFun ["x", "y"] (EVar "y")]) "∀a. (a, a) → a"
        runInferSpecCase (ELet "x" (EVar "id") $ ELet "y" (ELet "z" (ECall (EVar "x") [EVar "id"]) $ EVar "z") $ EVar "y") "∀a. a → a"
        runInferSpecCase (ECall (EVar "cons") [EVar "id", EVar "nil"]) "∀a. list[a → a]"
        runInferSpecCase (ECall (ECall (EVar "cons-curry") [EVar "id"]) [EVar "nil"]) "∀a. list[a → a]"
        runInferSpecCase (ELet "lst1" (ECall (EVar "cons") [EVar "id", EVar "nil"]) $ ELet "lst2" (ECall (EVar "cons") [EVar "succ", EVar "lst1"]) $ EVar "lst2") "list[int → int]"
        runInferSpecCase (ECall (ECall (EVar "cons-curry") [EVar "id"]) [ECall (ECall (EVar "cons-curry") [EVar "succ"]) [ECall (ECall (EVar "cons-curry") [EVar "id"]) [EVar "nil"]]]) "list[int → int]"
        failInferSpecCase (ECall (EVar "plus") [EVar "one", EVar "true"]) "cannot unify types int and bool"
        failInferSpecCase (ECall (EVar "plus") [EVar "one"]) "unexpected number of arguments"
        runInferSpecCase (EFun ["x"] $ ELet "y" (EVar "x") $ EVar "y") "∀a. a → a"
        runInferSpecCase (EFun ["x"] $ ELet "y" (ELet "z" (ECall (EVar "x") [EFun ["x"] $ EVar "x"]) $ EVar "z") $ EVar "y") "∀a,b. ((a → a) → b) → b"
        runInferSpecCase (EFun ["x"] $ EFun ["y"] $ ELet "x" (ECall (EVar "x") [EVar "y"]) $ ECall (EVar "x") [EVar "y"]) "∀a,b. (a → a → b) → a → b"
        runInferSpecCase (EFun ["x"] $ ELet "y" (EFun ["z"] $ ECall (EVar "x") [EVar "z"]) $ EVar "y") "∀a,b. (a → b) → a → b"
        runInferSpecCase (EFun ["x"] $ EFun ["y"] $ ELet "x" (ECall (EVar "x") [EVar "y"]) $ EFun ["x"] $ ECall (EVar "y") [EVar "x"]) "∀a,b,c. ((a → b) → c) → (a → b) → a → b"
        failInferSpecCase (EFun ["x"] $ ELet "y" (EVar "x") $ ECall (EVar "y") [EVar "y"]) "recursive types"
        failInferSpecCase (EFun ["x"] $ ECall (EVar "x") [EVar "x"]) "recursive types"
        failInferSpecCase (ECall (EVar "one") [EVar "id"]) "expected a function"
        runInferSpecCase (EFun ["x"] $ ELet "y" (EFun ["z"] $ EVar "z") $ ECall (EVar "y") [EVar "y"]) "∀a,b. a → b → b"
        runInferSpecCase (EFun ["f"] $ ELet "x" (EFun ["g", "y"] $ ELet "_" (ECall (EVar "g") [EVar "y"]) $ ECall (EVar "eq") [EVar "f", EVar "g"]) $ EVar "x") "∀a,b. (a → b) → (a → b, a) → bool"
        runInferSpecCase (ELet "const" (EFun ["x"] $ EFun ["y"] $ EVar "x") $ EVar "const") "∀a,b. a → b → a"
        runInferSpecCase (ELet "apply" (EFun ["f", "x"] $ ECall (EVar "f") [EVar "x"]) $ EVar "apply") "∀a,b. (a → b, a) → b"
        runInferSpecCase (ELet "apply-curry" (EFun ["f"] $ EFun ["x"] $ ECall (EVar "f") [EVar "x"]) $ EVar "apply-curry") "∀a,b. (a → b) → a → b"
        runInferSpecCase (ECall (EVar "apply") [EVar "succ", EVar "one"]) "int"
        runInferSpecCase (ECall (ECall (EVar "apply-curry") [EVar "succ"]) [EVar "one"]) "int"
        runInferSpecCase (ECall (EVar "single") [EVar "id"]) "∀a. list[a → a]"
