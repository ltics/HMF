{-# OPTIONS_GHC -Wall #-}

module Infer where

import Ast
import Type
import State
import qualified Data.Map as M
import Data.IORef
import System.IO.Unsafe(unsafePerformIO)
import Control.Exception
import Control.Monad

currentId :: IORef Int
currentId = createState 0

nextId :: Infer Int
nextId = do
    v <- readIORef currentId
    writeIORef currentId (v + 1)
    return v

resetId :: Infer ()
resetId = do
    writeIORef currentId 0

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
        return ()
    TApp fn args -> do
        occursCheckAdjustLevels tvId tvLevel fn
        mapM_ (occursCheckAdjustLevels tvId tvLevel) args
        return ()
    TVar var -> case readState var of
                      Unbound otherId otherLevel -> if otherId == tvId
                        then error "recursive types"
                        else if otherLevel > tvLevel
                                then do
                                  writeIORef var (Unbound otherId tvLevel)
                                  return ()
                                else return ()
                      Link t' -> do
                            occursCheckAdjustLevels tvId tvLevel t'
                            return ()
                      Generic _ -> assert False $ return ()

canNotUnifyError :: T -> T -> Infer ()
canNotUnifyError t1 t2 = error $ "cannot unify types " ++ show t1 ++ " and " ++ show t2

unify' :: T -> T -> Infer ()
unify' (TConst name1) (TConst name2) | name1 == name2 = return ()
unify' (TApp fn1 args1) (TApp fn2 args2) = do
    unify fn1 fn2
    zipWithM_ unify args1 args2
    return ()
unify' (TArrow params1 rtn1) (TArrow params2 rtn2) = do
    zipWithM_ unify params1 params2
    unify rtn1 rtn2
    return ()
unify' t1@(TVar ty1) t2@(TVar ty2) = case (readState ty1, readState ty2) of
                                        ((Unbound id1 _), (Unbound id2 _)) -> if id1 == id2
                                                                                then assert False return ()
                                                                                else canNotUnifyError t1 t2
                                        _ -> canNotUnifyError t1 t2
unify' t1@(TVar ty1) ty2 = case readState ty1 of
                            Link ty1' -> unify ty1' ty2
                            Unbound id1 level1 -> do
                                occursCheckAdjustLevels id1 level1 ty2
                                writeIORef ty1 $ Link ty2
                                return ()
                            _ -> canNotUnifyError t1 ty2
unify' ty1 t2@(TVar ty2) = case readState ty2 of
                            Link ty2' -> unify ty1 ty2'
                            Unbound id2 level2 -> do
                                occursCheckAdjustLevels id2 level2 ty1
                                writeIORef ty2 $ Link ty1
                            _ -> canNotUnifyError ty1 t2
unify' ty1 ty2 = canNotUnifyError ty1 ty2

unify :: T -> T -> Infer ()
unify ty1 ty2 = do
    if ty1 == ty2 then return () else unify' ty1 ty2

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
                        TVar var -> case readState var of
                                    Link t' -> generalize level t'
                                    Unbound i otherLevel -> if otherLevel > level
                                                                then return (TVar $ createState $ Generic i)
                                                                else return t
                                    _ -> return t
                        _ -> return t

instantiate :: Rank -> T -> Infer T
instantiate level t = do
    idVarMap <- newIORef (M.empty :: (M.Map Int T))
    let inst = f t where f ty = case ty of
                                TConst _ -> ty
                                TArrow params rtn -> TArrow (map f params) $ f rtn
                                TApp fn args -> TApp (f fn) (map f args)
                                TVar var -> case readState var of
                                            Unbound _ _ -> ty
                                            Link ty' -> f ty'
                                            Generic i -> let m = readState idVarMap in
                                                            case M.lookup i m of
                                                             Just var' -> var'
                                                             Nothing -> unsafePerformIO $ do
                                                                var' <- newVar level
                                                                modifyIORef idVarMap (\m' -> M.insert i var' m')
                                                                return var'
    return inst

matchFunType :: Int -> T -> ([T], T)
matchFunType numParams t = case t of
                            TArrow params rtn -> if length params /= numParams
                                                then error "unexpected number of arguments"
                                                else (params, rtn)
                            TVar var -> case readState var of
                                        Link ty -> matchFunType numParams ty
                                        Unbound _ level -> unsafePerformIO $ do
                                            paramTyList <- mapM (\_ -> newVar level) [1..numParams]
                                            rtnTy <- newVar level
                                            writeIORef var $ Link $ TArrow paramTyList rtnTy
                                            return (paramTyList, rtnTy)
                                        _ -> error "expected a function"
                            _ -> error "expected a function"

infer :: (M.Map Ast.Name T) -> Rank -> Expr -> Infer T
infer env level e = case e of
                        EVar name -> case M.lookup name env of
                                        Just t -> instantiate level t
                                        Nothing -> error $ "variable " ++ name ++ " not found"
                        EFun params body -> do
                            paramTyList <- mapM (\_ -> newVar level) params
                            let fnEnv = foldl (\env' (n, t) -> M.insert n t env') env $ zip params paramTyList
                            rtnTy <- infer fnEnv level body
                            return $ TArrow paramTyList rtnTy
                        ELet name value body -> do
                            valueTy <- infer env (level + 1) value
                            generalizedTy <- generalize level valueTy
                            infer (M.insert name generalizedTy env) level body
                        ECall fn args -> do
                            fnTy <- infer env level fn
                            let (paramTyList, rtnTy) = matchFunType (length args) fnTy
                            argTyList <- mapM (\argExpr -> infer env level argExpr) args
                            mapM_ (\(paramTy, argTy) -> unify paramTy argTy) $ zip paramTyList argTyList
                            return rtnTy
