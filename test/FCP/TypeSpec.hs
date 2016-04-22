module FCP.TypeSpec where

import FCP.Ast
import FCP.Type
import State
import qualified Text.PrettyPrint as PP
import Test.Hspec

spec :: Spec
spec = do
    describe "unlink type" $
      it "should remove internal link wrapper" $ do
        let ty = TVar $ createState $ Link $ TVar $ createState $ Link $ TVar $ createState $ Generic 3
        t <- unlink ty
        ty `shouldBe` (TVar $ createState $ Link $ TVar $ createState $ Generic 3)
        t `shouldBe` (TVar $ createState $ Generic 3)
    describe "unlink type" $
      it "should remove internal link wrapper" $ do
        let tvarA = TVar (createState (Bound 0))
        let tvarB = TVar (createState (Bound 1))
        let basicFunc = TForall [0, 1] $ TArrow [tvarA] tvarB
        let universalQuantifier = EAnn (EVar "x") (TAnn [] (TForall [0,1] (TArrow [tvarA] tvarB)))
        let existentialQuantifier = EAnn (EVar "x") (TAnn [0,1] (TArrow [tvarA] tvarB))
        (PP.text . show $ basicFunc) `shouldBe` PP.text "∀a,b. a → b"
        (PP.text . show $ universalQuantifier) `shouldBe` PP.text "x : ∀a,b. a → b"
        (PP.text . show $ existentialQuantifier) `shouldBe` PP.text "x : ∃a,b. a → b"
