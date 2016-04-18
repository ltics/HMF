module InferSpec where

import Type
import Infer
import State
import Data.IORef
import qualified Text.PrettyPrint as PP
import Test.Hspec

spec :: Spec
spec = do
    describe "create var" $ do
      it "should create vars" $ do
        params <- mapM (\_ -> newVar 0) [1..3]
        (PP.text . show $ params) `shouldBe` PP.text "[_0,_1,_2]"
    describe "unification test" $ do
      it "unify arrow type" $ do
        a <- newVar 0
        b <- newVar 0
        c <- newVar 0
        unify (TArrow [TConst "int", TConst "bool"] $ TConst "int") (TArrow [a, b] c)
        (PP.text . show $ a) `shouldBe` PP.text "int"
        (PP.text . show $ b) `shouldBe` PP.text "bool"
        (PP.text . show $ c) `shouldBe` PP.text "int"
      it "unify const type" $ do
        a <- newVar 0
        unify (TConst "int") a
        (PP.text . show $ a) `shouldBe` PP.text "int"
