{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TupleSections #-}

module Algw.Infer where

import Algw.Ast
import Algw.Type
import Algw.Env
import State
import Data.IORef
import Data.Maybe
import Control.Monad
import qualified Data.Map as M
import qualified Data.Set as S

makeNewVar :: Infer (Infer TName)
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
instantiate :: Infer TName -> Scheme -> Infer T
-- each poly type hold single quantified type variable is not really a good design, but just to be compatible with the origin paper
-- τ ::= α | ι | τ → τ
-- σ ::= τ | ∀α. σ
instantiate newVar t = let boundVars = allVars t `S.difference` freeVars t
                           -- update quantified type variable with fresh one
                           update acc a = do
                                        fresh <- fmap TVar newVar
                                        return $ M.insert a fresh acc
                           replace = foldM update M.empty boundVars
                       -- applicative functor
                       in pure (replaceFreeVars t) <*> replace

occurs :: TName -> T -> Bool
occurs a t = a `S.member` freeVars t

makeSingleSubrule :: TName -> T -> Infer Subrule
makeSingleSubrule a t
  | t == TVar a = return emptyRule
  | occurs a t = error "occurs check fails"
  | otherwise = return $ M.singleton a t

-- find mgu(most general unifier) of two types
unify :: T -> T -> Infer Subrule
unify TInt TInt = return emptyRule
unify TBool TBool = return emptyRule
unify (TVar n) t = makeSingleSubrule n t
unify t (TVar n) = makeSingleSubrule n t
unify (TArrow tl1 tr1) (TArrow tl2 tr2) = do
      s1 <- unify tl1 tl2
      s2 <- subst s1 tr1 `unify` subst s1 tr2
      return $ s2 `compose` s1
unify t1 t2 = error $ "types do not unify: " ++ show t1 ++ " vs. " ++ show t2

-- just like assoc in clojure
assocEnv :: TName -> Scheme -> Env -> Env
assocEnv n v env = M.insert n v $ M.delete n env

algw :: Infer TName -> Env -> Expr -> IO (Subrule, T)
algw newVar env (EVar name) = (emptyRule,) <$> instantiate newVar t -- pure (emptyRule,) <*> instantiate newVar t is also fine
  where t = fromMaybe (error $ "unbound variable: " ++ name) $ M.lookup name env

{-
t <- fmap TVar newVar will work because
instance Functor IO where
  fmap f action = do
    result <- action
    return (f result)
-}

algw newVar env (EAbs name expr) = do
  fresh <- fmap TVar newVar
  let env' = assocEnv name (Mono fresh) env
  (subrule, mono) <- algw newVar env' expr
  return (subrule, subst subrule fresh `TArrow` mono)

algw newVar env (EApp e1 e2) = do
  (s1, m1) <- algw newVar env e1
  (s2, m2) <- algw newVar (subst s1 env) e2
  fresh <- fmap TVar newVar
  s3 <- unify (subst s2 m1) (TArrow m2 fresh)
  return (s3 `compose` s2 `compose` s1, subst s3 fresh)

algw newVar env (ELet name value body) = do
  (s1, vmono) <- algw newVar env value
  let env' = subst s1 env
      g = generalize env' vmono
      env'' = assocEnv name g env'
  (s2, bmono) <- algw newVar env'' body
  return (s2 `compose` s1, bmono)

-- environment is assumptions at the initial state
infer :: Env -> Expr -> IO T
infer env expr =
  do newVar <- makeNewVar
     (_, t) <- algw newVar env expr
     return t
