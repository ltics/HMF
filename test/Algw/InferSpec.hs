module Algw.InferSpec where

import Algw.Ast
import Algw.Type
import Algw.Infer
import Algw.Env
import qualified Text.PrettyPrint as PP
import qualified Data.Map as M
import Test.Hspec

runInferSpecCase :: Expr -> String -> IO ()
runInferSpecCase expr expect = do
    t <- infer assumptions expr
    let gt = generalize M.empty t
    (PP.text . show $ gt) `shouldBe` PP.text expect

failInferSpecCase :: Expr -> String -> IO ()
failInferSpecCase expr error = infer assumptions expr `shouldThrow` errorCall error

spec :: Spec
spec = do
    describe "unification test" $
      it "should find mgu" $ do
        let mono1 = TArrow (TVar "a") TInt
        let mono2 = TArrow (TVar "b") $ TVar "b"
        let mono3 = TArrow (TVar "a") $ TVar "b"
        let mono4 = TArrow (TArrow (TVar "b") $ TVar "c") $ TVar "c"
        subrule1 <- unify mono1 mono2
        subrule2 <- unify mono3 mono4
        subrule1 `shouldBe` M.fromList [("a", TInt), ("b", TInt)]
        subrule2 `shouldBe` M.fromList [("a", TArrow (TVar "c") $ TVar "c"), ("b", TVar "c")]
    describe "inference test" $
      it "should infer most general or principal types for given expression" $ do
        runInferSpecCase (EVar "id") "∀a. a → a"
        runInferSpecCase (EApp (EVar "id") $ EApp (EVar "id") (EVar "one")) "int"
        runInferSpecCase (EApp (EApp (EVar "eq") (EVar "false")) (EVar "true")) "bool"
        runInferSpecCase (EVar "compose") "∀a. ∀b. ∀c. (b → c) → (a → b) → a → c"
        runInferSpecCase (EApp (EVar "compose") (EVar "not")) "∀a. (a → bool) → a → bool"
        runInferSpecCase (EApp (EApp (EVar "compose") (EVar "not")) (EApp (EVar "eq") (EVar "one"))) "int → bool"
        runInferSpecCase (EApp (EVar "compose") (EApp (EVar "add") (EVar "one"))) "∀a. (a → int) → a → int"
        runInferSpecCase (EApp (EApp (EApp (EVar "compose") (EVar "eq")) (EVar "add")) (EVar "one")) "(int → int) → bool" -- a really interesting case
        runInferSpecCase (EApp (EVar "compose") (EVar "compose")) "∀a. ∀d. ∀e. ∀f. (a → e → f) → a → (d → e) → d → f"
        failInferSpecCase (EApp (EVar "one") (EVar "one")) "types do not unify: int vs. int → a"
        failInferSpecCase (EAbs "f" $ EApp (EVar "f") (EVar "f")) "occurs check fails"
        failInferSpecCase (EApp (EApp (EVar "add") (EVar "true")) (EVar "false")) "types do not unify: int vs. bool"
        failInferSpecCase (EVar "x") "unbound variable: x"
        runInferSpecCase (EApp (EVar "id") (EApp (EVar "id") (EVar "one"))) "int"
        runInferSpecCase (EAbs "a"
                            (ELet "x"
                                (EAbs "b"
                                      (ELet "y"
                                            (EAbs "c" (EApp (EVar "a") (EVar "zero")))
                                            (EApp (EVar "y") (EVar "one"))))
                                (EApp (EVar "x") (EVar "one")))) "∀h. (int → h) → h"
        failInferSpecCase (EAbs "a" (EAbs "b"
                                          (EApp (EVar "b")
                                                (EApp (EVar "a")
                                                      (EApp (EVar "a") (EVar "b")))))) "occurs check fails"

        runInferSpecCase (EApp (EApp (EVar "choose")
                                     (EAbs "a"
                                           (EAbs "b"
                                                 (EVar "a"))))
                               (EAbs "a"
                                     (EAbs "b"
                                           (EVar "b")))) "∀f. f → f → f"
        runInferSpecCase (EAbs "x"
                               (EAbs "y"
                               (ELet "x"
                                     (EApp (EVar "x")
                                           (EVar "y"))
                                     (EAbs "x"
                                           (EApp (EVar "y")
                                                 (EVar "x")))))) "∀c. ∀d. ∀e. ((d → e) → c) → (d → e) → d → e"
