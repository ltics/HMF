{-# OPTIONS_GHC -Wall #-}

module Algw.Type where

import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

type TName = String

data T = TInt
       | TBool -- these two can also use TConst to make it more extendable
       | TVar TName
       | TArrow T T
  deriving (Eq, Ord)

instance Show T where
  show TInt = "int"
  show TBool = "bool"
  show (TVar name) = name
  show (TArrow t1@(TArrow _ _) t2) = "(" ++ show t1 ++ ")" ++ " → " ++ show t2
  show (TArrow t1 t2) = show t1 ++ " → " ++ show t2

-- principle type schemes
data Scheme = Mono T --monomorphic types
            | Poly TName Scheme -- polymorphic types
  deriving (Eq, Ord)

instance Show Scheme where
  show (Mono t) = show t
  show (Poly a t) = "∀" ++ a ++ ". " ++ show t

-- a substitution rule is just mapping type variables to mono types
type Subrule = M.Map TName T

emptyRule :: Subrule
emptyRule = M.empty

-- subrule composition s2 ∘ s1 t = s2 (s1 t)
compose :: Subrule -> Subrule -> Subrule
compose s s' = M.map (subst s) s' `M.union` s

class TypeVars a where
  allVars :: a -> S.Set TName
  freeVars :: a -> S.Set TName
  subst :: Subrule -> a -> a

instance TypeVars T where
  allVars (TVar a) = S.singleton a
  allVars (TArrow t1 t2) = allVars t1 `S.union` allVars t2
  allVars _ = S.empty

  freeVars = allVars

  -- use default value
  subst s v@(TVar n) = fromMaybe v $ M.lookup n s
  subst s (TArrow t1 t2) = subst s t1 `TArrow` subst s t2
  subst _ t = t

instance TypeVars Scheme where
  allVars (Mono t) = allVars t
  allVars (Poly a t) = S.insert a $ allVars t

  freeVars (Mono t) = freeVars t
  freeVars (Poly a t) = S.delete a $ freeVars t

  subst s (Mono t) = Mono $ subst s t
  subst s (Poly a t) = Poly a $ subst (M.delete a s) t
