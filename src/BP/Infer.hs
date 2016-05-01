{-# OPTIONS_GHC -Wall #-}

module BP.Infer where

import BP.Ast
import BP.Type
import BP.Env
import State
import Data.IORef
import Text.Read (readMaybe)
import Control.Monad (when, zipWithM_)
import Control.Monad.Loops (anyM)
import qualified Data.Map as M
import qualified Data.Set as S

type NonGeneric = (S.Set Type)

prune :: Type -> Infer Type
prune t = case t of
            TypeVariable _ inst _ -> do
              instV <- readIORef inst
              case instV of
                Just inst' -> do
                  newInstance <- prune inst'
                  writeIORef inst $ Just newInstance
                  return newInstance
                Nothing -> return t
            _ -> return t

occursInType :: Type -> Type -> Infer Bool
occursInType v t = do
  tP <- prune t
  case tP of
    TypeOperator _ ts -> occursIn v ts
    v' -> return $ v == v'

occursIn :: Type -> [Type] -> Infer Bool
occursIn t = anyM (occursInType t)

isGeneric :: Type -> NonGeneric -> Infer Bool
isGeneric t nonGeneric = not <$> (occursIn t $ S.toList nonGeneric)

fresh :: Type -> NonGeneric -> Infer Type
fresh t nonGeneric = do
  mappings <- newIORef M.empty -- A mapping of TypeVariables to TypeVariables
  let freshrec ty = prune ty >>= (\tyP -> case tyP of
                                          TypeVariable _ _ _ -> do
                                            isG <- isGeneric tyP nonGeneric
                                            if isG
                                            then do
                                              m <- readIORef mappings
                                              case M.lookup tyP m of
                                                Just tVar -> return tVar
                                                Nothing -> do
                                                  newVar <- makeVariable
                                                  modifyIORef mappings $ M.insert tyP newVar
                                                  return newVar
                                            else return tyP
                                          TypeOperator name types -> do
                                            newTypes <- mapM freshrec types
                                            return $ TypeOperator name newTypes)
  freshrec t

getType :: TName -> Env -> NonGeneric -> Infer Type
getType name env nonGeneric = case M.lookup name env of
                                Just var -> fresh var nonGeneric
                                Nothing -> case (readMaybe name :: Maybe Int) of
                                            Just _ -> return intT
                                            Nothing -> error $ "Undefined symbol " ++ name

unify :: Type -> Type -> Infer ()
unify t1 t2 = do
  t1P <- prune t1
  t2P <- prune t2
  case (t1P, t2P) of
    (a@(TypeVariable _ inst _), b) -> when (a /= b) $ do
                                          isOccurs <- occursInType a b
                                          when isOccurs $ error "Recusive unification"
                                          writeIORef inst $ Just b
    (a@(TypeOperator _ _), b@(TypeVariable _ _ _)) -> unify b a
    (a@(TypeOperator name1 types1), b@(TypeOperator name2 types2)) -> if name1 /= name2 || (length types1) /= (length types2)
                                                                     then error $ "Type mismatch " ++ show a ++ " ≠ " ++ show b
                                                                     else zipWithM_ unify types1 types2

analyze :: Term -> Env -> NonGeneric -> Infer Type
analyze term env nonGeneric = case term of
                                Ident name -> getType name env nonGeneric
                                Apply fn arg -> do
                                  fnT <- analyze fn env nonGeneric
                                  argT <- analyze arg env nonGeneric
                                  rtnT <- makeVariable
                                  unify (functionT argT rtnT) fnT
                                  return rtnT
                                Lambda arg body -> do
                                  argT <- makeVariable
                                  rtnT <- analyze body (M.insert arg argT env) (S.insert argT nonGeneric) -- non generic on lambda arg type
                                  return $ functionT argT rtnT
                                Let n def body -> do
                                  defT <- analyze def env nonGeneric
                                  analyze body (M.insert n defT env) nonGeneric
                                LetRec n def body -> do
                                  newT <- makeVariable
                                  let newEnv = M.insert n newT env
                                  defT <- analyze def newEnv (S.insert newT nonGeneric)
                                  unify newT defT
                                  analyze body newEnv nonGeneric
