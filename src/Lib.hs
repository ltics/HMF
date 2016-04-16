module Lib
    ( someFunc
    ) where

import Ast
import Type
import Infer

someFunc :: IO ()
someFunc = putStrLn "someFunc"
