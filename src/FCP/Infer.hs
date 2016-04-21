{-# OPTIONS_GHC -Wall #-}

module FCP.Infer where

import FCP.Type
import State
import Data.IORef
import qualified Data.Map as M
import qualified Data.Set as S

newVar :: Rank -> Infer T
newVar level = do
    next <- nextId
    return $ TVar $ createState $ Unbound next level

newGenVar :: Infer T
newGenVar = do
    next <- nextId
    return $ TVar $ createState $ Generic next

newBoundVar :: Infer (Id, T)
newBoundVar = do
    next <- nextId
    return (next, TVar $ createState $ Bound next)

type IdType = M.Map Id T
type NameType = M.Map Name T

idTypeMapFrom2Lists :: [Id] -> [T] -> IdType
idTypeMapFrom2Lists ids tys = foldl (\m (i, t) -> M.insert i t m) M.empty $ zip ids tys

idTypeMapRemoveAll :: IdType -> [Id] -> IdType
idTypeMapRemoveAll idTy ids = foldl (\m i -> M.delete i m) idTy ids

occursCheckAdjustLevels :: Int -> Int -> T -> Infer ()
occursCheckAdjustLevels tvId tvLevel t = case t of
    TConst _ -> return ()
    TArrow params rtn -> do
        mapM_ (occursCheckAdjustLevels tvId tvLevel) params
        occursCheckAdjustLevels tvId tvLevel rtn
    TApp fn args -> do
        occursCheckAdjustLevels tvId tvLevel fn
        mapM_ (occursCheckAdjustLevels tvId tvLevel) args
    TForall _ t' -> occursCheckAdjustLevels tvId tvLevel t'
    TVar var -> do
        varV <- readIORef var
        case varV of
            Unbound otherId otherLevel -> if otherId == tvId
                then error "recursive types"
                else if otherLevel > tvLevel
                      then do
                        writeIORef var (Unbound otherId tvLevel)
                      else return ()
            Link t' -> do
                  occursCheckAdjustLevels tvId tvLevel t'
            _ -> return ()

substitudeBoundVars' :: IdType -> T -> Infer T
substitudeBoundVars' idTy t = case t of
                                TConst _ -> return t
                                TApp fn args -> do
                                    fnT <- substitudeBoundVars' idTy fn
                                    argsT <- mapM (substitudeBoundVars' idTy) args
                                    return $ TApp fnT argsT
                                TArrow params rtn -> do
                                    paramsT <- mapM (substitudeBoundVars' idTy) params
                                    rtnT <- substitudeBoundVars' idTy rtn
                                    return $ TArrow paramsT rtnT
                                TForall varIds ty -> do
                                    tyT <- substitudeBoundVars' (idTypeMapRemoveAll idTy varIds) ty
                                    return $ TForall varIds tyT
                                TVar var -> do
                                    varV <- readIORef var
                                    case varV of
                                        Link ty -> substitudeBoundVars' idTy ty
                                        Bound i -> case M.lookup i idTy of
                                                    Just ty -> return ty
                                                    Nothing -> return t
                                        _ -> return t

substitudeBoundVars :: [Id] -> [T] -> T -> Infer T
substitudeBoundVars varIds types t = substitudeBoundVars' (idTypeMapFrom2Lists varIds types) t

freeGenericVars :: T -> Infer (S.Set T)
freeGenericVars t = do
    freeVarSet <- newIORef S.empty
    let f ty = case ty of
                TConst _ -> return ()
                TApp fn args -> do
                    f fn
                    mapM_ f args
                TArrow params rtn -> do
                    mapM_ f params
                    f rtn
                TForall _ ty' -> f ty'
                TVar var -> do
                    varV <- readIORef var
                    case varV of
                        Link ty' -> f ty'
                        Generic _ -> modifyIORef freeVarSet (\s -> S.insert ty s)
                        _ -> return ()
    f t
    freeVarSetV <- readIORef freeVarSet
    return freeVarSetV
