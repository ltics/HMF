{-# OPTIONS_GHC -Wall #-}

module Infer where

-- import Ast
import Type
import State
import Data.IORef
import System.IO.Unsafe(unsafePerformIO)
import Control.Exception
import Control.Monad

currentId :: IORef Int
currentId = createState 0

nextId :: IO Int
nextId = do
    v <- readIORef currentId
    writeIORef currentId (v + 1)
    return v

resetId :: IO ()
resetId = do
    writeIORef currentId 0

newVar :: Rank -> T
newVar level =
    TVar $ createState $ Unbound (unsafePerformIO nextId) level

newGenVar :: () -> T
newGenVar () = TVar $ createState $ Generic (unsafePerformIO nextId)

occursCheckAdjustLevels :: Int -> Int -> T -> IO ()
occursCheckAdjustLevels tvId tvLevel t = case t of
    (TConst _) -> return ()
    (TArrow params rtn) -> do
        _ <- mapM_ (occursCheckAdjustLevels tvId tvLevel) params
        _ <- occursCheckAdjustLevels tvId tvLevel rtn
        return ()
    (TApp ty tys) -> do
        _ <- occursCheckAdjustLevels tvId tvLevel ty
        _ <- mapM_ (occursCheckAdjustLevels tvId tvLevel) tys
        return ()
    (TVar var) -> case readState var of
                      (Unbound otherId otherLevel) -> if otherId == tvId
                        then error "recursive types"
                        else if otherLevel > tvLevel
                                then do
                                  _ <- writeIORef var (Unbound otherId tvLevel)
                                  return ()
                                else return ()
                      (Link t') -> do
                            _ <- occursCheckAdjustLevels tvId tvLevel t'
                            return ()
                      (Generic _) -> assert False $ return ()

unify' :: T -> T -> IO ()
unify' (TConst name1) (TConst name2) | name1 == name2 = return ()
unify' (TApp fn1 args1) (TApp fn2 args2) = do
    _ <- unify fn1 fn2
    _ <- zipWithM_ unify' args1 args2
    return ()

unify :: T -> T -> IO ()
unify ty1 ty2 = if ty1 == ty2 then return () else unify' ty1 ty2
