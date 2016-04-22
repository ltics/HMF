module FCP.InferSpec where

import FCP.Ast
import FCP.Type
import FCP.Infer
import FCP.Env
import State (resetId)
import qualified Text.PrettyPrint as PP
import Test.Hspec

runInferSpecCase :: Expr -> String -> IO ()
runInferSpecCase expr expect = do
    t <- infer assumptions 0 expr
    gt <- generalize (-1) t
    resetId
    (PP.text . show $ gt) `shouldBe` PP.text expect

spec :: Spec
spec = do
    describe "create var" $
      it "should create vars" $ do
        params <- mapM (\ _ -> newVar 0) [1..3]
        resetId
        (PP.text . show $ params) `shouldBe` PP.text "[@unknown0,@unknown1,@unknown2]"
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
        infer assumptions 0 (EVar "x") `shouldThrow` errorCall "variable x not found"
        infer assumptions 0 (ELet "x" (EVar "x") (EVar "x")) `shouldThrow` errorCall "variable x not found"
        runInferSpecCase (ELet "x" (EVar "id") $ EVar "x") "∀a. a → a"
        runInferSpecCase (ELet "x" (EFun [EParam "y" Nothing] $ EVar "y") $ EVar "x") "∀a. a → a"
        runInferSpecCase (EFun [EParam "x" Nothing] $ EVar "x") "∀a. a → a"
        runInferSpecCase (EVar "pair") "∀a,b. (a, b) → pair[a, b]"
        runInferSpecCase (EFun [EParam "x" Nothing] $ ELet "y" (EFun [EParam "z" Nothing] $ EVar "z") $ EVar "y") "∀a,b. a → b → b"
        runInferSpecCase (EFun [EParam "x" Nothing] $ ELet "y" (EFun [EParam "z" Nothing] $ EVar "x") $ EVar "y") "∀a,b. a → b → a"
        runInferSpecCase (ELet "f" (EFun [EParam "x" Nothing] $ EVar "x") $ ELet "id" (EFun [EParam "y" Nothing] $ EVar "y") $ ECall (EVar "eq") [EVar "f", EVar "id"]) "bool"
        runInferSpecCase (ELet "f" (EFun [EParam "x" Nothing] $ EVar "x") $ ELet "id" (EFun [EParam "y" Nothing] $ EVar "y") $ ECall (ECall (EVar "eq-curry") [EVar "f"]) [EVar "id"]) "bool"
        runInferSpecCase (ELet "f" (EFun [EParam "x" Nothing] $ EVar "x") $ ECall (EVar "eq") [EVar "f", EVar "succ"]) "bool"
        runInferSpecCase (ELet "f" (EFun [EParam "x" Nothing] $ EVar "x") $ ECall (ECall (EVar "eq-curry") [EVar "f"]) [EVar "succ"]) "bool"
        -- let polymorphism
        runInferSpecCase (ELet "f" (EFun [EParam "x" Nothing] $ EVar "x") $ ECall (EVar "pair") [ECall (EVar "f") [EVar "one"], ECall (EVar "f") [EVar "true"]]) "pair[int, bool]"
        infer assumptions 0 (EFun [EParam "f" Nothing] $ ECall (EVar "pair") [ECall (EVar "f") [EVar "one"], ECall (EVar "f") [EVar "true"]]) `shouldThrow` errorCall "cannot unify types int and bool"
        runInferSpecCase (ELet "f" (EFun [EParam "x" Nothing, EParam "y" Nothing] $ ELet "a" (ECall (EVar "eq") [EVar "x", EVar "y"]) $ ECall (EVar "eq") [EVar "x", EVar "y"]) $ EVar "f") "∀a. (a, a) → bool"
        runInferSpecCase (ELet "f" (EFun [EParam "x" Nothing, EParam "y" Nothing] $ ELet "a" (ECall (ECall (EVar "eq-curry") [EVar "x"]) [EVar "y"]) $ ECall (ECall (EVar "eq-curry") [EVar "x"]) [EVar "y"]) $ EVar "f") "∀a. (a, a) → bool"
        runInferSpecCase (ECall (EVar "id") [EVar "id"]) "∀a. a → a"
        runInferSpecCase (ECall (EVar "choose") [EFun [EParam "x" Nothing, EParam "y" Nothing] (EVar "x"), EFun [EParam "x" Nothing, EParam "y" Nothing] (EVar "y")]) "∀a. (a, a) → a"
        runInferSpecCase (ECall (ECall (EVar "choose-curry") [EFun [EParam "x" Nothing, EParam "y" Nothing] (EVar "x")]) [EFun [EParam "x" Nothing, EParam "y" Nothing] (EVar "y")]) "∀a. (a, a) → a"
        runInferSpecCase (ELet "x" (EVar "id") $ ELet "y" (ELet "z" (ECall (EVar "x") [EVar "id"]) $ EVar "z") $ EVar "y") "∀a. a → a"
        runInferSpecCase (ECall (EVar "cons") [EVar "id", EVar "nil"]) "∀a. list[a → a]"
        runInferSpecCase (ECall (ECall (EVar "cons-curry") [EVar "id"]) [EVar "nil"]) "∀a. list[a → a]"
        runInferSpecCase (ELet "lst1" (ECall (EVar "cons") [EVar "id", EVar "nil"]) $ ELet "lst2" (ECall (EVar "cons") [EVar "succ", EVar "lst1"]) $ EVar "lst2") "list[int → int]"
        runInferSpecCase (ECall (ECall (EVar "cons-curry") [EVar "id"]) [ECall (ECall (EVar "cons-curry") [EVar "succ"]) [ECall (ECall (EVar "cons-curry") [EVar "id"]) [EVar "nil"]]]) "list[int → int]"
        infer assumptions 0 (ECall (EVar "plus") [EVar "one", EVar "true"]) `shouldThrow` errorCall "cannot unify types int and bool"
        infer assumptions 0 (ECall (EVar "plus") [EVar "one"]) `shouldThrow` errorCall "unexpected number of arguments"
        runInferSpecCase (EFun [EParam "x" Nothing] $ ELet "y" (EVar "x") $ EVar "y") "∀a. a → a"
        runInferSpecCase (EFun [EParam "x" Nothing] $ ELet "y" (ELet "z" (ECall (EVar "x") [EFun [EParam "x" Nothing] $ EVar "x"]) $ EVar "z") $ EVar "y") "∀a,b. ((a → a) → b) → b"
        runInferSpecCase (EFun [EParam "x" Nothing] $ EFun [EParam "y" Nothing] $ ELet "x" (ECall (EVar "x") [EVar "y"]) $ ECall (EVar "x") [EVar "y"]) "∀a,b. (a → a → b) → a → b"
        runInferSpecCase (EFun [EParam "x" Nothing] $ ELet "y" (EFun [EParam "z" Nothing] $ ECall (EVar "x") [EVar "z"]) $ EVar "y") "∀a,b. (a → b) → a → b"
        runInferSpecCase (EFun [EParam "x" Nothing] $ EFun [EParam "y" Nothing] $ ELet "x" (ECall (EVar "x") [EVar "y"]) $ EFun [EParam "x" Nothing] $ ECall (EVar "y") [EVar "x"]) "∀a,b,c. ((a → b) → c) → (a → b) → a → b"
        infer assumptions 0 (EFun [EParam "x" Nothing] $ ELet "y" (EVar "x") $ ECall (EVar "y") [EVar "y"]) `shouldThrow` errorCall "recursive types"
        infer assumptions 0 (EFun [EParam "x" Nothing] $ ECall (EVar "x") [EVar "x"]) `shouldThrow` errorCall "recursive types"
        infer assumptions 0 (ECall (EVar "one") [EVar "id"]) `shouldThrow` errorCall "expected a function"
        runInferSpecCase (EFun [EParam "x" Nothing] $ ELet "y" (EFun [EParam "z" Nothing] $ EVar "z") $ ECall (EVar "y") [EVar "y"]) "∀a,b. a → b → b"
        runInferSpecCase (EFun [EParam "f" Nothing] $ ELet "x" (EFun [EParam "g" Nothing, EParam "y" Nothing] $ ELet "_" (ECall (EVar "g") [EVar "y"]) $ ECall (EVar "eq") [EVar "f", EVar "g"]) $ EVar "x") "∀a,b. (a → b) → (a → b, a) → bool"
        runInferSpecCase (ELet "const" (EFun [EParam "x" Nothing] $ EFun [EParam "y" Nothing] $ EVar "x") $ EVar "const") "∀a,b. a → b → a"
        runInferSpecCase (ELet "apply" (EFun [EParam "f" Nothing, EParam "x" Nothing] $ ECall (EVar "f") [EVar "x"]) $ EVar "apply") "∀a,b. (a → b, a) → b"
        runInferSpecCase (ELet "apply-curry" (EFun [EParam "f" Nothing] $ EFun [EParam "x" Nothing] $ ECall (EVar "f") [EVar "x"]) $ EVar "apply-curry") "∀a,b. (a → b) → a → b"
        runInferSpecCase (ECall (EVar "apply") [EVar "succ", EVar "one"]) "int"
        runInferSpecCase (ECall (ECall (EVar "apply-curry") [EVar "succ"]) [EVar "one"]) "int"
        runInferSpecCase (ECall (EVar "single") [EVar "id"]) "∀a. list[a → a]"
