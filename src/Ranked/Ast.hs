{-# OPTIONS_GHC -Wall #-}

module Ranked.Ast where

import Data.List (intercalate)

type Name = String

data Expr = EVar Name
          | EFun [Name] Expr
          | ECall Expr [Expr]
          | ELet Name Expr Expr

instance Show Expr where
    show (EVar name) = name
    show (EFun params body) = "ƒ " ++ unwords params ++ " → " ++ show body
    show (ECall fn args) = show fn ++ "(" ++ intercalate ", " (map show args) ++ ")"
    show (ELet name value body) = "let " ++ name ++ " = " ++ show value ++ " in " ++ show body
