module InferSpec where

import Type
import Infer
import State
import Data.IORef
import Test.Hspec

spec :: Spec
spec = do
    describe "create var" $ do
      it "should create vars" $ do
        params <- mapM (\_ -> newVar 0) [1..3]
        show params `shouldBe` "[\"_0\",\"_1\",\"_2\"]"
    describe "unification test" $ do
      it "unify arrow type" $ do
        a <- newVar 0
        b <- newVar 0
        c <- newVar 0
        unify (TArrow [TConst "int", TConst "bool"] $ TConst "int") (TArrow [a, b] c)
        show a `shouldBe` "\"int\""
        show b `shouldBe` "\"bool\""
        show c `shouldBe` "\"int\""
      it "unify const type" $ do
        a <- newVar 0
        unify (TConst "int") a
        show a `shouldBe` "\"int\""
