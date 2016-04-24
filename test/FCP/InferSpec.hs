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
    -- no need to generalize here
    -- gt <- generalize (-1) t
    resetId
    (PP.text . show $ t) `shouldBe` PP.text expect

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
    describe "classic inference test" $
      it "should infer most general or principal types for given expression" $ do
        runInferSpecCase (EVar "id") "∀a. a → a"
        runInferSpecCase (EVar "one") "int"
        failInferSpecCase (EVar "x") "variable x not found"
        failInferSpecCase (ELet "x" (EVar "x") (EVar "x")) "variable x not found"
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
        failInferSpecCase (EFun [EParam "f" Nothing] $ ECall (EVar "pair") [ECall (EVar "f") [EVar "one"], ECall (EVar "f") [EVar "true"]]) "cannot unify types int and bool"
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
        failInferSpecCase (ECall (EVar "plus") [EVar "one", EVar "true"]) "cannot unify types int and bool"
        failInferSpecCase (ECall (EVar "plus") [EVar "one"]) "unexpected number of arguments"
        runInferSpecCase (EFun [EParam "x" Nothing] $ ELet "y" (EVar "x") $ EVar "y") "∀a. a → a"
        runInferSpecCase (EFun [EParam "x" Nothing] $ ELet "y" (ELet "z" (ECall (EVar "x") [EFun [EParam "x" Nothing] $ EVar "x"]) $ EVar "z") $ EVar "y") "∀a,b. ((a → a) → b) → b"
        runInferSpecCase (EFun [EParam "x" Nothing] $ EFun [EParam "y" Nothing] $ ELet "x" (ECall (EVar "x") [EVar "y"]) $ ECall (EVar "x") [EVar "y"]) "∀a,b. (a → a → b) → a → b"
        runInferSpecCase (EFun [EParam "x" Nothing] $ ELet "y" (EFun [EParam "z" Nothing] $ ECall (EVar "x") [EVar "z"]) $ EVar "y") "∀a,b. (a → b) → a → b"
        runInferSpecCase (EFun [EParam "x" Nothing] $ EFun [EParam "y" Nothing] $ ELet "x" (ECall (EVar "x") [EVar "y"]) $ EFun [EParam "x" Nothing] $ ECall (EVar "y") [EVar "x"]) "∀a,b,c. ((a → b) → c) → (a → b) → a → b"
        failInferSpecCase (EFun [EParam "x" Nothing] $ ELet "y" (EVar "x") $ ECall (EVar "y") [EVar "y"]) "recursive types"
        failInferSpecCase (EFun [EParam "x" Nothing] $ ECall (EVar "x") [EVar "x"]) "recursive types"
        failInferSpecCase (ECall (EVar "one") [EVar "id"]) "expected a function"
        runInferSpecCase (EFun [EParam "x" Nothing] $ ELet "y" (EFun [EParam "z" Nothing] $ EVar "z") $ ECall (EVar "y") [EVar "y"]) "∀a,b. a → b → b"
        runInferSpecCase (EFun [EParam "f" Nothing] $ ELet "x" (EFun [EParam "g" Nothing, EParam "y" Nothing] $ ELet "_" (ECall (EVar "g") [EVar "y"]) $ ECall (EVar "eq") [EVar "f", EVar "g"]) $ EVar "x") "∀a,b. (a → b) → (a → b, a) → bool"
        runInferSpecCase (ELet "const" (EFun [EParam "x" Nothing] $ EFun [EParam "y" Nothing] $ EVar "x") $ EVar "const") "∀a,b. a → b → a"
        runInferSpecCase (ELet "apply" (EFun [EParam "f" Nothing, EParam "x" Nothing] $ ECall (EVar "f") [EVar "x"]) $ EVar "apply") "∀a,b. (a → b, a) → b"
        runInferSpecCase (ELet "apply-curry" (EFun [EParam "f" Nothing] $ EFun [EParam "x" Nothing] $ ECall (EVar "f") [EVar "x"]) $ EVar "apply-curry") "∀a,b. (a → b) → a → b"
        runInferSpecCase (ECall (EVar "apply") [EVar "succ", EVar "one"]) "int"
        runInferSpecCase (ECall (ECall (EVar "apply-curry") [EVar "succ"]) [EVar "one"]) "int"
        runInferSpecCase (ECall (EVar "single") [EVar "id"]) "∀a. list[a → a]"
    describe "HMF inference test" $
      it "should support first class polymorphism" $ do
        runInferSpecCase (EVar "ids") "list[∀a. a → a]"
        failInferSpecCase (EFun [EParam "y" Nothing] $ ECall (EVar "pair") [ECall (EVar "y") [EVar "one"], ECall (EVar "y") [EVar "true"]]) "cannot unify types int and bool"
        -- support parametric polymorphism
        runInferSpecCase (EFun [EParam "y" (Just (TAnn [] (TForall [0] $ TArrow [tvarA] tvarA)))] $ ECall (EVar "pair") [ECall (EVar "y") [EVar "one"], ECall (EVar "y") [EVar "true"]]) "(∀a. a → a) → pair[int, bool]"
        runInferSpecCase (ECall (EVar "cons") [EVar "ids", EVar "nil"]) "list[list[∀a. a → a]]"
        runInferSpecCase (ECall (EVar "choose") [EVar "nil", EVar "ids"]) "list[∀a. a → a]"
        runInferSpecCase (ECall (EVar "choose") [EVar "ids", EVar "nil"]) "list[∀a. a → a]"
        runInferSpecCase (ECall (EVar "cons") [EFun [EParam "x" Nothing] (EVar "x"), EVar "ids"]) "list[∀a. a → a]"
        runInferSpecCase (ELet "rev_cons" (EFun [EParam "x" Nothing, EParam "y" Nothing] $ ECall (EVar "cons") [EVar "y", EVar "x"]) $ ECall (EVar "rev_cons") [EVar "ids", EVar "id"]) "list[∀a. a → a]"
        runInferSpecCase (ECall (EVar "cons") [EVar "id", EVar "ids"]) "list[∀a. a → a]"
        runInferSpecCase (ECall (EVar "cons") [EVar "id", ECall (EVar "cons") [EVar "succ", EVar "nil"]]) "list[int → int]"
        runInferSpecCase (ECall (EVar "poly") [EVar "id"]) "pair[int, bool]"
        runInferSpecCase (ECall (EVar "poly") [EFun [EParam "x" Nothing] $ EVar "x"]) "pair[int, bool]"
        failInferSpecCase (ECall (EVar "poly") [EVar "succ"]) "cannot unify types @generic0 and int"
        runInferSpecCase (ECall (EVar "apply") [EVar "succ", EVar "one"]) "int"
        runInferSpecCase (ECall (EVar "rev-apply") [EVar "one", EVar "succ"]) "int"
        runInferSpecCase (ECall (EVar "apply") [EVar "poly", EVar "id"]) "pair[int, bool]"
        runInferSpecCase (ECall (ECall (EVar "apply-curry") [EVar "poly"]) [EVar "id"]) "pair[int, bool]"
        runInferSpecCase (ECall (EVar "rev-apply") [EVar "id", EVar "poly"]) "pair[int, bool]"
        failInferSpecCase (ECall (ECall (EVar "rev-apply-curry") [EVar "id"]) [EVar "poly"]) "cannot unify types @unknown4 → @unknown4 and ∀a. a → a"
        -- have to add explicit type annotation in function calls
        runInferSpecCase (ECall (ECall (EVar "rev-apply-curry") [EAnn (EVar "id") (TAnn [] (TForall [0] (TArrow [tvarA] tvarA)))]) [EVar "poly"]) "pair[int, bool]"
        runInferSpecCase (EAnn (EAnn (EVar "id") (TAnn [] (TForall [0] $ TArrow [tvarA] tvarA))) $ TAnn [] $ TArrow [tcInt] tcInt) "int → int"
        runInferSpecCase (EAnn (EVar "id") $ TAnn [] $ TArrow [tcInt] tcInt) "int → int"
        -- single(id : forall[a] a -> a) is extremely different from single(id), note pos of quantifier
        runInferSpecCase (ECall (EVar "single") [EAnn (EVar "id") $ TAnn [] (TForall [0] $ TArrow [tvarA] tvarA)]) "list[∀a. a → a]"
        runInferSpecCase (ECall (EVar "id->id") [EVar "id"]) "∀a. a → a"
        runInferSpecCase (ECall (EVar "almost-id-id") [EVar "id"]) "∀a. a → a"
        runInferSpecCase (EFun [EParam "x" $ Just (TAnn [] (TForall [0] $ TArrow [tvarA] tvarA))] $ EVar "x") "∀a. (∀b. b → b) → a → a"
        -- power of first class polymorphism ƒ x -> x(x) ƒ x:∀a. a → a → x(x)
        runInferSpecCase (EFun [EParam "f" $ Just $ TAnn [] (TForall [0] $ TArrow [tvarA] tvarA)] $ ECall (EVar "f") [EVar "f"]) "∀a. (∀b. b → b) → a → a"
        failInferSpecCase (EFun [EParam "id" Nothing] $ ECall (EVar "poly") $ [EVar "id"]) "type @generic1 → @generic1 is not an instance of ∀a. a → a"
        failInferSpecCase (EFun [EParam "ids" Nothing] $ ECall (EVar "id-ids") $ [EVar "ids"]) "polymorphic parameter inferred: list[∀a. a → a]"
        runInferSpecCase (ECall (EVar "poly") [ECall (EVar "id") [EVar "id"]]) "pair[int, bool]"
        runInferSpecCase (ECall (EVar "length") [EVar "ids"]) "int"
        runInferSpecCase (EFun [EParam "x" Nothing] $ EFun [EParam "y" Nothing] $ ELet "z" (ECall (EVar "choose") [EVar "x", EVar "y"]) (EVar "z")) "∀a. a → a → a"
        -- note the position of quantifier
        runInferSpecCase (ECall (EFun [EParam "x" Nothing] $ EFun [EParam "y" Nothing] $ ELet "z" (ECall (EVar "choose") [EVar "x", EVar "y"]) (EVar "z")) [EAnn (EVar "id") $ TAnn [] (TForall [0] $ TArrow [tvarA] tvarA)]) "(∀a. a → a) → ∀a. a → a"
        runInferSpecCase (ECall (EFun [EParam "x" Nothing] $ EFun [EParam "y" Nothing] $ ELet "z" (ECall (EVar "choose") [EVar "x", EVar "y"]) (EVar "z")) [EVar "id"]) "∀a. (a → a) → a → a"
        runInferSpecCase (ECall (EVar "map") [EVar "head", ECall (EVar "single") [EVar "ids"]]) "list[∀a. a → a]"
        runInferSpecCase (ECall (ECall (EVar "map-curry") [EVar "head"]) [ECall (EVar "single") [EVar "ids"]]) "list[∀a. a → a]"
        runInferSpecCase (ECall (EVar "apply") [ECall (EVar "map-curry") [EVar "head"], ECall (EVar "single") [EVar "ids"]]) "list[∀a. a → a]"
        runInferSpecCase (ECall (ECall (EVar "apply-curry") [ECall (EVar "map-curry") [EVar "head"]]) [ECall (EVar "single") [EVar "ids"]]) "list[∀a. a → a]"
        runInferSpecCase (ECall (EVar "apply") [EVar "id", EVar "one"]) "int"
        runInferSpecCase (ECall (ECall (EVar "apply-curry") [EVar "id"]) [EVar "one"]) "int"
        -- this is really magic that bound type variable A and B in magic can unify to the same one
        runInferSpecCase (ECall (EVar "poly") [EVar "magic"]) "pair[int, bool]"
        runInferSpecCase (ECall (EVar "id-magic") [EVar "magic"]) "∀a,b. a → b"
        failInferSpecCase (ECall (EVar "id-magic") [EVar "id"]) "cannot unify types @generic1 and @generic2"
        -- univeral quantifier
        runInferSpecCase (EFun [EParam "f" $ Just (TAnn [] (TForall [0, 1] $ TArrow [tvarA] tvarB))] $ ELet "a" (ECall (EVar "id-magic") [EVar "f"]) $ EVar "one") "(∀a,b. a → b) → int"
        -- existential quantifier
        failInferSpecCase (EFun [EParam "f" $ Just (TAnn [0, 1] $ TArrow [tvarA] tvarB)] $ ECall (EVar "id-magic") [EVar "f"]) "type @generic3 → @generic2 is not an instance of ∀a,b. a → b"
        -- unify bound type variables
        runInferSpecCase (EFun [EParam "f" $ Just (TAnn [] (TForall [0, 1] $ TArrow [tvarA] tvarB))] $ EAnn (EVar "f") $ TAnn [] (TForall [0] (TArrow [tvarA] tvarA))) "(∀a,b. a → b) → ∀a. a → a"
        runInferSpecCase (ELet "const" (EAnn (EVar "any") $ TAnn [] (TForall [0] $ TArrow [tvarA] (TForall [1] $ TArrow [tvarB] tvarA))) $ ECall (EVar "const") [EVar "any"]) "∀a,b. a → b"
