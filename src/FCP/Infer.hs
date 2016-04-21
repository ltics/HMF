{-# OPTIONS_GHC -Wall #-}

module FCP.Infer where

import FCP.Type
import State
import Data.IORef
import Control.Monad
import Control.Exception
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
idTypeMapRemoveAll = foldl $ flip M.delete

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
            Unbound otherId otherLevel | otherId == tvId -> error "recursive types"
                                       | otherLevel > tvLevel -> writeIORef var $ Unbound otherId tvLevel
                                       | otherwise -> return ()
            Link t' -> occursCheckAdjustLevels tvId tvLevel t'
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
substitudeBoundVars varIds types = substitudeBoundVars' (idTypeMapFrom2Lists varIds types)

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
                        Generic _ -> modifyIORef freeVarSet $ S.insert ty
                        _ -> return ()
    f t
    readIORef freeVarSet

escapeCheck :: [T] -> T -> T -> Infer Bool
escapeCheck genericVars t1 t2 = do
    freeVars1 <- freeGenericVars t1
    freeVars2 <- freeGenericVars t2
    return $ any (\var -> S.member var freeVars1 || S.member var freeVars2) genericVars

canNotUnifyError :: T -> T -> Infer ()
canNotUnifyError t1 t2 = error $ "cannot unify types " ++ show t1 ++ " and " ++ show t2

unify' :: T -> T -> Infer ()
unify' (TConst name1) (TConst name2) | name1 == name2 = return ()
unify' (TApp fn1 args1) (TApp fn2 args2) = do
    unify fn1 fn2
    zipWithM_ unify args1 args2
unify' (TArrow params1 rtn1) (TArrow params2 rtn2) = do
    zipWithM_ unify params1 params2
    unify rtn1 rtn2
unify' t1@(TVar ty1) t2@(TVar ty2) = do
    ty1V <- readIORef ty1
    ty2V <- readIORef ty2
    case (ty1V, ty2V) of
        (Unbound id1 level1, Unbound id2 _) -> if id1 == id2
                                                then assert False return ()
                                                else do
                                                    occursCheckAdjustLevels id1 level1 t2
                                                    writeIORef ty1 $ Link t2
        (Bound _, _) -> assert False return ()
        (_, Bound _) -> assert False return ()
        (Unbound id1 level1, _) -> do
            occursCheckAdjustLevels id1 level1 t2
            writeIORef ty1 $ Link t2
        (Link ty1', _) -> unify ty1' t2
        (_, Unbound id2 level2) -> do
            occursCheckAdjustLevels id2 level2 t1
            writeIORef ty2 $ Link t1
        (_, Link ty2') -> unify t1 ty2'
        _ -> canNotUnifyError t1 t2
unify' t1@(TVar ty1) ty2 = do
    ty1V <- readIORef ty1
    case ty1V of
        Link ty1' -> unify ty1' ty2
        Unbound id1 level1 -> do
            occursCheckAdjustLevels id1 level1 ty2
            writeIORef ty1 $ Link ty2
        _ -> canNotUnifyError t1 ty2
unify' ty1 t2@(TVar ty2) = do
    ty2V <- readIORef ty2
    case ty2V of
        Link ty2' -> unify ty1 ty2'
        Unbound id2 level2 -> do
            occursCheckAdjustLevels id2 level2 ty1
            writeIORef ty2 $ Link ty1
        _ -> canNotUnifyError ty1 t2
unify' ft1@(TForall varIds1 t1) ft2@(TForall varIds2 t2) = if length varIds1 /= length varIds2
                                                           then canNotUnifyError t1 t2
                                                           else do
                                                            genericVars <- mapM (const newGenVar) varIds1
                                                            let genericVars' = reverse genericVars
                                                            genericTy1 <- substitudeBoundVars varIds1 genericVars' t1
                                                            genericTy2 <- substitudeBoundVars varIds2 genericVars' t2
                                                            unify genericTy1 genericTy2
                                                            isEscape <- escapeCheck genericVars' ft1 ft2
                                                            when isEscape $ canNotUnifyError ft1 ft2
unify' ty1 ty2 = canNotUnifyError ty1 ty2

unify :: T -> T -> Infer ()
unify ty1 ty2 = unless (ty1 == ty2) $ unify' ty1 ty2
