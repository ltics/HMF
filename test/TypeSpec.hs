module TypeSpec where

import Type
import State
import Data.IORef
import Test.Hspec

spec :: Spec
spec = do
    describe "type stringify test" $ do
      it "stringify arrow type" $
        show (TArrow [TVar (createState (Generic 3)), TVar (createState (Generic 4))] $ TVar (createState (Generic 5))) `shouldBe` "\"\\8704a,b,c. (a, b) \\8594 c\""
      it "stringify const type" $
        show (TConst "int") `shouldBe` "\"int\""
    describe "type equality test" $ do
      it "eq" $
        (TVar $ createState $ Generic 3) == (TVar $ createState $ Generic 3) `shouldBe` True
      it "eq" $
        [(TVar $ createState $ Generic 3)] == [(TVar $ createState $ Generic 3)] `shouldBe` True
      it "eq" $
        (TArrow [TVar (createState (Generic 3)), TVar (createState (Generic 4))] $ TVar (createState (Generic 5)))
         == (TArrow [TVar (createState (Generic 3)), TVar (createState (Generic 4))] $ TVar (createState (Generic 5)))
         `shouldBe` True
      it "not eq" $
        (TVar $ createState $ Generic 3) == (TVar $ createState $ Generic 5) `shouldBe` False
      it "not eq" $
        [(TVar $ createState $ Generic 3)] == [(TVar $ createState $ Generic 5)] `shouldBe` False
      it "not eq" $
        (TArrow [TVar (createState (Generic 3)), TVar (createState (Generic 4))] $ TVar (createState (Generic 5)))
         == (TArrow [TVar (createState (Generic 3)), TVar (createState (Generic 4))] $ TVar (createState (Generic 6)))
         `shouldBe` False

