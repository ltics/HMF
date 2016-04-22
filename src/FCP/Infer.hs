{-# OPTIONS_GHC -Wall #-}

module FCP.Infer where

import FCP.Ast
import FCP.Type hiding (Name)
import FCP.Env
import State
import Data.Function (on)
import Data.List (sortBy, intercalate)
import Data.IORef
import Control.Monad
import Control.Monad.Loops (allM)
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

canNotUnifyError :: T -> T -> Infer ()
canNotUnifyError t1 t2 = error $ "cannot unify types " ++ show t1 ++ " and " ++ show t2

notInstanceError :: T -> T -> Infer ()
notInstanceError t1 t2 = error $ "type " ++ show t1 ++ " is not an instance of " ++ show t2

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

substituteWithNewVars :: Rank -> [Id] -> T -> Infer ([T], T)
substituteWithNewVars level varIds t = do
    varTs <- mapM (const $ newVar level) varIds
    tyT <- substitudeBoundVars (reverse varIds) varTs t
    return (varTs, tyT)

instantiateTypeAnn :: Rank -> TAnn -> Infer ([T], T)
instantiateTypeAnn _ (TAnn [] t) = return ([], t)
instantiateTypeAnn level (TAnn varIds t) = substituteWithNewVars level varIds t


instantiate :: Rank -> T -> Infer T
instantiate level (TForall varIds t) = do
    (_, instantiatedT) <- substituteWithNewVars level varIds t
    return instantiatedT
instantiate level t@(TVar var) = do
    varV <- readIORef var
    case varV of
        Link t' -> instantiate level t'
        _ -> return t
instantiate _ t = return t

subsume :: Rank -> T -> T -> Infer ()
subsume level t1 t2 = do
    instantiatedT2 <- instantiate level t2
    unlinkedT1 <- unlink t1
    case unlinkedT1 of
        TForall varIds1 t1' -> do
            genericVars <- mapM (const newGenVar) varIds1
            let genericVars' = reverse genericVars
            genericT1 <- substitudeBoundVars varIds1 genericVars' t1'
            unify genericT1 instantiatedT2
            isEscape <- escapeCheck genericVars' unlinkedT1 t2
            when isEscape $ notInstanceError t2 unlinkedT1
        _ -> unify unlinkedT1 instantiatedT2


generalize :: Rank -> T -> Infer T
generalize level t = do
    varIdsRevRef <- newIORef []
    let f ty = case ty of
                TConst _ -> return ()
                TForall _ ty' -> f ty'
                TArrow params rtn -> do
                    mapM_ f params
                    f rtn
                TApp fn args -> do
                    f fn
                    mapM_ f args
                TVar var -> do
                    varV <- readIORef var
                    case varV of
                        Link ty' -> f ty'
                        Generic _ -> assert False return ()
                        Bound _ -> return ()
                        Unbound otherId otherLevel | otherLevel > level -> do
                                                                        writeIORef var $ Bound otherId
                                                                        varIdsRevRefV <- readIORef varIdsRevRef
                                                                        unless (otherId `elem` varIdsRevRefV) $ writeIORef varIdsRevRef (otherId : varIdsRevRefV)
                                                   | otherwise -> return ()
    f t
    varIdsRevRefV <- readIORef varIdsRevRef
    case varIdsRevRefV of
        [] -> return t
        varIdRevs -> return $ TForall (reverse varIdRevs) t

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
                                        Just t -> return t
                                        Nothing -> error $ "variable " ++ name ++ " not found"
                        EFun params body -> do
                            fnEnvRef <- newIORef env
                            varListRef <- newIORef []
                            paramTs <- mapM (\(EParam name maybeAnn) -> do
                                                paramT <- case maybeAnn of
                                                            Nothing -> do
                                                                varT <- newVar (level + 1)
                                                                modifyIORef varListRef (\l -> varT : l)
                                                                return varT
                                                            Just ann -> do
                                                                (varTs, annT) <- instantiateTypeAnn (level + 1) ann
                                                                modifyIORef varListRef (\l -> varTs ++ l)
                                                                return annT
                                                modifyIORef fnEnvRef $ M.insert name paramT
                                                return paramT)
                                            params
                            fnEnv <- readIORef fnEnvRef
                            inferedRtnT <- infer fnEnv (level + 1) body
                            rtnT <- if isAnnotated body
                                    then return inferedRtnT
                                    else instantiate (level + 1) inferedRtnT
                            varList <- readIORef varListRef
                            isAllMonomorphic <- allM isMonomorphic varList
                            if isAllMonomorphic
                            then generalize level $ TArrow paramTs rtnT
                            else error ("polymorphic parameter inferred: " ++ intercalate ", " (map show varList))
                        ELet name value body -> do
                            valueT <- infer env (level + 1) value
                            infer (M.insert name valueT env) level body
                        ECall fn args -> do
                            fnTy <- infer env (level + 1) fn
                            fnTy' <- instantiate (level + 1) fnTy
                            (paramTs, rtnT) <- matchFunType (length args) fnTy'
                            inferArgs env (level + 1) paramTs args
                            rtnT' <- instantiate (level + 1) rtnT
                            generalize level rtnT'
                        EAnn expr ann -> do
                            (_, annT) <- instantiateTypeAnn level ann
                            exprT <- infer env level expr
                            subsume level annT exprT
                            return annT

getOrdering :: T -> Infer Int
getOrdering ty = do
  unlinkedT <- unlink ty
  case unlinkedT of
    TVar var -> do
        varV <- readIORef var
        case varV of
            Unbound _ _ -> return 1
            _ -> return 0
    _ -> return 0

inferArgs :: Env -> Rank ->  [T] -> [Expr] -> Infer ()
inferArgs env level paramTs args = do
    let pairs = zip paramTs args
    pairsWithOrd <- mapM (\(p, a) -> do
                             ord <- getOrdering p
                             return ((p, a), ord))
                    pairs
    let sortedPairs = map fst $ sortBy (compare `on` snd) pairsWithOrd
    mapM_ (\(paramT, argE) -> do
              argT <- infer env level argE
              if isAnnotated argE
              then unify paramT argT
              else subsume level paramT argT)
          sortedPairs
