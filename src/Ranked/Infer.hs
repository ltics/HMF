{-# OPTIONS_GHC -Wall #-}

module Ranked.Infer where

import Ranked.Ast
import Ranked.Type
import Ranked.Env
import State
import qualified Data.Map as M
import Data.IORef
import Control.Exception
import Control.Monad

newVar :: Rank -> Infer T
newVar level = do
    next <- nextId
    return $ TVar $ createState $ Unbound next level

newGenVar :: Infer T
newGenVar = do
    next <- nextId
    return $ TVar $ createState $ Generic next

occursCheckAdjustLevels :: Int -> Int -> T -> Infer ()
occursCheckAdjustLevels tvId tvLevel t = case t of
    TConst _ -> return ()
    TArrow params rtn -> do
        mapM_ (occursCheckAdjustLevels tvId tvLevel) params
        occursCheckAdjustLevels tvId tvLevel rtn
    TApp fn args -> do
        occursCheckAdjustLevels tvId tvLevel fn
        mapM_ (occursCheckAdjustLevels tvId tvLevel) args
    TVar var -> do
        varV <- readIORef var
        case varV of
            Unbound otherId otherLevel | otherId == tvId -> error "recursive types"
                                       | otherLevel > tvLevel -> writeIORef var $ Unbound otherId tvLevel
                                       | otherwise -> return ()
            Link t' -> occursCheckAdjustLevels tvId tvLevel t'
            Generic _ -> assert False return ()

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
unify' ty1 ty2 = canNotUnifyError ty1 ty2

unify :: T -> T -> Infer ()
unify ty1 ty2 = unless (ty1 == ty2) $ unify' ty1 ty2

generalize :: Rank -> T -> Infer T
generalize level t = case t of
                        TArrow params rtn -> do
                            paramTyList <- mapM (generalize level) params
                            rtnTy <- generalize level rtn
                            return $ TArrow paramTyList rtnTy
                        TApp fn args -> do
                            fnTy <- generalize level fn
                            argTyList <- mapM (generalize level) args
                            return $ TApp fnTy argTyList
                        TVar var -> do
                            varV <- readIORef var
                            case varV of
                                Link t' -> generalize level t'
                                Unbound i otherLevel -> if otherLevel > level
                                                            then return (TVar $ createState $ Generic i)
                                                            else return t
                                _ -> return t
                        _ -> return t

instantiate :: Rank -> T -> Infer T
instantiate level t = do
    idVarMap <- newIORef M.empty
    let f ty = case ty of
                TConst _ -> return ty
                TArrow params rtn -> do
                    paramsV <- mapM f params
                    rtnV <- f rtn
                    return $ TArrow paramsV rtnV
                TApp fn args -> do
                    fnV <- f fn
                    argsV <- mapM f args
                    return $ TApp fnV argsV
                TVar var -> do
                    varV <- readIORef var
                    case varV of
                        Unbound _ _ -> return ty
                        Link ty' -> f ty'
                        Generic i -> do
                            m <- readIORef idVarMap
                            case M.lookup i m of
                                Just var' -> return var'
                                Nothing -> do
                                   var' <- newVar level
                                   modifyIORef idVarMap $ M.insert i var'
                                   return var'
    f t

matchFunType :: Int -> T -> Infer ([T], T)
matchFunType numParams t = case t of
                            TArrow params rtn ->
                                if length params /= numParams
                                then error "unexpected number of arguments"
                                else return (params, rtn)
                            TVar var -> do
                                varV <- readIORef var
                                case varV of
                                    Link ty -> matchFunType numParams ty
                                    Unbound _ level -> do
                                        paramTyList <- mapM (const $ newVar level) [1..numParams]
                                        rtnTy <- newVar level
                                        writeIORef var $ Link $ TArrow paramTyList rtnTy
                                        return (paramTyList, rtnTy)
                                    _ -> error "expected a function"
                            _ -> error "expected a function"

infer :: Env -> Rank -> Expr -> Infer T
infer env level e = case e of
                        EVar name -> case M.lookup name env of
                                        Just t -> instantiate level t
                                        Nothing -> error $ "variable " ++ name ++ " not found"
                        EFun params body -> do
                            paramTyList <- mapM (const $ newVar level) params
                            let fnEnv = foldl (\env' (n, t) -> M.insert n t env') env $ zip params paramTyList
                            rtnTy <- infer fnEnv level body
                            return $ TArrow paramTyList rtnTy
                        ELet name value body -> do
                            valueTy <- infer env (level + 1) value
                            generalizedTy <- generalize level valueTy
                            infer (M.insert name generalizedTy env) level body
                        ECall fn args -> do
                            fnTy <- infer env level fn
                            (paramTyList, rtnTy) <- matchFunType (length args) fnTy
                            argTyList <- mapM (infer env level) args
                            mapM_ (uncurry unify) $ zip paramTyList argTyList
                            return rtnTy
