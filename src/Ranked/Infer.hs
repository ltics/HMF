{-# OPTIONS_GHC -Wall #-}

module Ranked.Infer where

import Ranked.Ast
import Ranked.Type
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
            Unbound otherId otherLevel -> if otherId == tvId
                then error "recursive types"
                else if otherLevel > tvLevel
                      then writeIORef var (Unbound otherId tvLevel)
                      else return ()
            Link t' -> do
                  occursCheckAdjustLevels tvId tvLevel t'
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
        ((Unbound id1 level1), (Unbound id2 _)) -> if id1 == id2
                                                then assert False return ()
                                                else do
                                                    occursCheckAdjustLevels id1 level1 t2
                                                    writeIORef ty1 $ Link t2
        ((Unbound id1 level1), _) -> do
            occursCheckAdjustLevels id1 level1 t2
            writeIORef ty1 $ Link t2
        ((Link ty1'), _) -> do
            unify ty1' t2
        (_, (Unbound id2 level2)) -> do
            occursCheckAdjustLevels id2 level2 t1
            writeIORef ty2 $ Link t1
        (_, (Link ty2')) -> do
            unify t1 ty2'
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
                        TVar var -> do
                            varV <- readIORef var
                            case varV of
                                Link t' -> generalize level t'
                                Unbound i otherLevel -> if otherLevel > level
                                                            then do
                                                               return (TVar $ createState $ Generic i)
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
                                   modifyIORef idVarMap (\m' -> M.insert i var' m')
                                   return var'
    inst <- f t
    return inst

matchFunType :: Int -> T -> Infer ([T], T)
matchFunType numParams t = case t of
                            TArrow params rtn -> do
                                if length params /= numParams
                                then error "unexpected number of arguments"
                                else return (params, rtn)
                            TVar var -> do
                                varV <- readIORef var
                                case varV of
                                    Link ty -> matchFunType numParams ty
                                    Unbound _ level -> do
                                        paramTyList <- mapM (\_ -> newVar level) [1..numParams]
                                        rtnTy <- newVar level
                                        writeIORef var $ Link $ TArrow paramTyList rtnTy
                                        return (paramTyList, rtnTy)
                                    _ -> error "expected a function"
                            _ -> error "expected a function"

infer :: M.Map Ranked.Ast.Name T -> Rank -> Expr -> Infer T
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
                            (paramTyList, rtnTy) <- matchFunType (length args) fnTy
                            argTyList <- mapM (\argExpr -> infer env level argExpr) args
                            mapM_ (\(paramTy, argTy) -> unify paramTy argTy) $ zip paramTyList argTyList
                            return rtnTy

tcInt :: T
tcInt = TConst "int"

tcBool :: T
tcBool = TConst "bool"

tcList :: T
tcList = TConst "list"

tcPair :: T
tcPair = TConst "pair"

tvarA :: T
tvarA = TVar (createState (Generic 0))

tvarB :: T
tvarB = TVar (createState (Generic 1))

polyList :: T
polyList = TApp tcList [tvarA]

polyList' :: T
polyList' = TApp tcList [tvarB]

polyPair :: T
polyPair = TApp tcPair [tvarA, tvarB]

assumptions :: M.Map Ranked.Ast.Name T
assumptions = M.fromList
    [("head", TArrow [polyList] tvarA),
     ("tail", TArrow [polyList] polyList),
     ("nil", polyList),
     ("cons", TArrow [tvarA, polyList] polyList),
     ("cons-curry", TArrow [tvarA] $ TArrow [polyList] polyList),
     ("map", TArrow [TArrow [tvarA] tvarB, polyList] polyList'),
     ("map-curry", TArrow [TArrow [tvarA] tvarB] $ TArrow [polyList] polyList'),
     ("single", TArrow [tvarA] polyList),
     ("zero", tcInt),
     ("one", tcInt),
     ("succ", TArrow [tcInt] tcInt),
     ("plus", TArrow [tcInt, tcInt] tcInt),
     ("true", tcBool),
     ("false", tcBool),
     ("not", TArrow [tcBool] tcBool),
     ("eq", TArrow [tvarA, tvarA] tcBool),
     ("eq-curry", TArrow [tvarA] $ TArrow [tvarA] tcBool),
     ("pair", TArrow [tvarA, tvarB] polyPair),
     ("pair-curry", TArrow [tvarA] $ TArrow [tvarB] polyPair),
     ("first", TArrow [polyPair] tvarA),
     ("second", TArrow [polyPair] tvarB),
     ("id", TArrow [tvarA] tvarA),
     ("const", TArrow [tvarA] $ TArrow [tvarB] tvarA),
     ("apply", TArrow [TArrow [tvarA] tvarB, tvarA] tvarB),
     ("apply-curry", TArrow [TArrow [tvarA] tvarB] $ TArrow [tvarA] tvarB),
     ("choose", TArrow [tvarA, tvarA] tvarA),
     ("choose-curry", TArrow [tvarA] $ TArrow [tvarA] tvarA)]