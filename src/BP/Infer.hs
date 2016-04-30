{-# OPTIONS_GHC -Wall #-}

module BP.Infer where

import BP.Type
import State
import Data.IORef
import Control.Monad (when, zipWithM_)
import Control.Monad.Loops (anyM)

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
                                                                     then error $ "Type mismatch " ++ show a ++ " /= " ++ show b
                                                                     else zipWithM_ unify types1 types2
