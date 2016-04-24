module Algw.InferSpec where

import Algw.Ast
import Algw.Type
import Algw.Infer
import qualified Data.Map as M
import Test.Hspec

spec :: Spec
spec = do
    describe "unification test" $
      it "should find mgu" $ do
        let mono1 = TArrow (TVar "a") TInt
        let mono2 = TArrow (TVar "b") $ TVar "b"
        let mono3 = TArrow (TVar "a") $ TVar "b"
        let mono4 = TArrow (TArrow (TVar "b") $ TVar "c") $ TVar "c"
        unify mono1 mono2 `shouldBe` M.fromList [("a", TInt),("b", TInt)]
        unify mono3 mono4 `shouldBe` M.fromList [("a", TArrow (TVar "c") $ TVar "c"),("b", TVar "c")]

