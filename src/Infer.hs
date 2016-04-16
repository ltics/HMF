{-# OPTIONS_GHC -Wall #-}

module Infer where

import Ast
import Type
import State
import Data.IORef
import System.IO.Unsafe(unsafePerformIO)

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
