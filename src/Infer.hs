{-# OPTIONS_GHC -Wall #-}

module Infer where

import Ast
import Type
import Data.IORef
import System.IO.Unsafe(unsafePerformIO)

currentId :: IORef Int
currentId = unsafePerformIO $ newIORef 0

nextId :: IO Int
nextId = do
    v <- readIORef currentId
    _ <- putStrLn $ show v
    writeIORef currentId (v + 1)
    return v

resetId :: IO ()
resetId = do
    writeIORef currentId 0

newVar :: Rank -> T
newVar level =
    TVar $ unsafePerformIO $ newIORef $ Unbound (unsafePerformIO nextId) level

newGenVar :: () -> T
newGenVar () = TVar $ unsafePerformIO $ newIORef $ Generic (unsafePerformIO nextId)
