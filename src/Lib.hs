module Lib
    ( someFunc
    ) where

import Ranked.Ast
import Ranked.Type
import Ranked.Infer
import State (Infer)
import qualified Text.PrettyPrint as PP

uni :: Infer T
uni = do
    a <- newVar 0
    b <- newVar 0
    c <- newVar 0
    unify (TArrow [TConst "int", TConst "bool"] $ TConst "int") (TArrow [a, b] c)
    return $ TArrow [a, b] c

someFunc :: IO ()
someFunc = do
    t <- uni
    putStrLn $ show t