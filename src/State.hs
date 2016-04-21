module State where

import Data.IORef
import System.IO.Unsafe(unsafePerformIO)

createState :: a -> IORef a
createState = unsafePerformIO . newIORef

readState :: IORef a -> a
readState = unsafePerformIO . readIORef

type Infer a  = IO a

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
