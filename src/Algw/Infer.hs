{-# LANGUAGE FlexibleInstances #-}

module Algw.Infer where

import Algw.Ast
import Algw.Type
import State
import Data.IORef
import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S

makeNewVar :: IO (IO String)
makeNewVar =
  do r <- newIORef 'a'
     -- return a closure like structure to mock a generator
     return $ do
       v <- readIORef r
       modifyIORef r succ
       return [v]

generalize :: Env -> T -> Scheme
generalize env t = let fvs = freeVars t `S.difference` freeVars env
                   -- cause poly type here can only hold single quantified type var
                   in S.fold Poly (Mono t) fvs

replaceFreeVars :: Scheme -> Subrule -> T
replaceFreeVars (Mono t) s = subst s t
replaceFreeVars (Poly _ t) s = replaceFreeVars t s

-- just replace quantified type variables by fresh ones to make it monomorphic
instantiate :: Infer String -> Scheme -> Infer T
-- each poly type hold single quantified type variable is not really a good design, but just to be compatible with the origin paper
-- τ ::= α | ι | τ → τ
-- σ ::= τ | ∀α. σ
instantiate newVar t = let boundVars = allVars t `S.difference` freeVars t
                           -- update quantified type variable with fresh one
                           update acc a = do
                                        n <- fmap TVar newVar
                                        return $ M.insert a n acc
                           replace = foldM update M.empty boundVars
                       -- applicative functor
                       in pure (replaceFreeVars t) <*> replace

-- find mgu(most general unifier) of two types
unify :: T -> T -> Subrule
unify TInt TInt = emptyRule
unify TBool TBool = emptyRule
--unify (TVar n) t =
