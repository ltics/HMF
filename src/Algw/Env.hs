{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Algw.Env where

import Algw.Ast (EName)
import Algw.Type (TypeVars(..), Scheme(..), T(..))
import qualified Data.Map as M
import qualified Data.Set as S

-- an environment is mapping variables to type schemes
type Env = M.Map EName Scheme

instance TypeVars Env where
  allVars = M.foldl (\avs t -> S.union avs $ allVars t) S.empty
  freeVars = M.foldl (\fvs t -> S.union fvs $ freeVars t) S.empty
  subst s = M.map (subst s)

assumptions :: Env
assumptions = M.fromList
    [("zero", Mono TInt),
     ("one", Mono TInt),
     ("true", Mono TBool),
     ("false", Mono TBool),
     ("not", Mono $ TBool `TArrow` TBool),
     ("and", Mono $ TArrow TBool $ TArrow TBool TBool),
     ("add", Mono $ TArrow TInt $ TArrow TInt TInt),
     ("id", Poly "a" $ Mono $ TVar "a" `TArrow` TVar "a"),
     ("eq", Poly "a" $ Mono $ TArrow (TVar "a") $ TArrow (TVar "a") TBool),
     ("compose", Poly "a" $ Poly "b" $ Poly "c" $ Mono $
        (TVar "b" `TArrow` TVar "c") `TArrow`
        ((TVar "a" `TArrow` TVar "b") `TArrow` (TVar "a" `TArrow` TVar "c"))),
     ("choose", Poly "a" $ Mono $ TVar "a" `TArrow` (TVar "a" `TArrow` TVar "a")),
     ("wrong", Poly "a" $ Mono $ TVar "a")]
