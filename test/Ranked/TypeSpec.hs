module Ranked.TypeSpec where

import Ranked.Type
import Ranked.Infer (newGenVar)
import State (createState, resetId)
import qualified Text.PrettyPrint as PP
import Test.Hspec

spec :: Spec
spec = do
    describe "type stringify test" $ do
      it "stringify arrow type" $ do
        a <- newGenVar
        b <- newGenVar
        c <- newGenVar
        resetId
        (PP.text . show $ TArrow [a, b] c) `shouldBe` PP.text "∀a,b,c. (a, b) → c"
      it "stringify const type" $
        (PP.text . show $ TConst "int") `shouldBe` PP.text "int"
    describe "type equality test" $ do
      it "eq" $ do
        (TVar $ createState $ Generic 3) == (TVar $ createState $ Generic 3) `shouldBe` True
        [(TVar $ createState $ Generic 3)] == [(TVar $ createState $ Generic 3)] `shouldBe` True
        (TArrow [TVar (createState (Generic 3)), TVar (createState (Generic 4))] $ TVar (createState (Generic 5)))
            == (TArrow [TVar (createState (Generic 3)), TVar (createState (Generic 4))] $ TVar (createState (Generic 5)))
            `shouldBe` True
      it "not eq" $ do
        (TVar $ createState $ Generic 3) == (TVar $ createState $ Generic 5) `shouldBe` False
        [(TVar $ createState $ Generic 3)] == [(TVar $ createState $ Generic 5)] `shouldBe` False
        (TArrow [TVar (createState (Generic 3)), TVar (createState (Generic 4))] $ TVar (createState (Generic 5)))
            == (TArrow [TVar (createState (Generic 3)), TVar (createState (Generic 4))] $ TVar (createState (Generic 6)))
            `shouldBe` False
