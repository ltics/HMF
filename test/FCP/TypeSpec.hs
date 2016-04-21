module FCP.TypeSpec where

import FCP.Type
import State
import Test.Hspec

spec :: Spec
spec =
    describe "unlink type" $
      it "should remove internal link wrapper" $ do
        let ty = TVar $ createState $ Link $ TVar $ createState $ Link $ TVar $ createState $ Generic 3
        t <- unlink ty
        ty `shouldBe` (TVar $ createState $ Link $ TVar $ createState $ Generic 3)
        t `shouldBe` (TVar $ createState $ Generic 3)
