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

