module State where

import Data.IORef
import System.IO.Unsafe(unsafePerformIO)

createState :: a -> IORef a
createState = unsafePerformIO . newIORef

readState :: IORef a -> a
readState = unsafePerformIO . readIORef

