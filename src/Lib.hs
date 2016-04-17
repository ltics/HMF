module Lib
    ( someFunc
    ) where

import Ast
import Type
import State
import Infer

ttt :: Infer String
ttt = do
    a <- newVar 0
    b <- newVar 0
    c <- newVar 0
    unify (TArrow [TConst "int", TConst "bool"] $ TConst "int") (TArrow [a, b] c)
    return $ show a

someFunc :: IO ()
someFunc = do
    t <- ttt
    putStrLn $ "type -> " ++ t
