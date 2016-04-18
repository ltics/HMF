module TypeSpec where

import Type
import State
import Data.IORef
import qualified Text.PrettyPrint as PP
import Test.Hspec

spec :: Spec
spec = do
    describe "type stringify test" $ do
      it "stringify arrow type" $
        (PP.text . show $ TArrow [TVar (createState (Generic 3)), TVar (createState (Generic 4))] $ TVar (createState (Generic 5))) `shouldBe` PP.text "∀a,b,c. (a, b) → c"
      it "stringify const type" $
        (PP.text . show $ TConst "int") `shouldBe` PP.text "int"
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
