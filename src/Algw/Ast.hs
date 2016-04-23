{-# OPTIONS_GHC -Wall #-}

module Algw.Ast where

type EName = String

data Expr = EVar EName
          | EAbs EName Expr -- lambda abstraction
          | EApp Expr Expr -- application
          | ELet EName Expr Expr -- let binding

instance Show Expr where
  show (EVar name) = name
  show (EAbs name expr) = "λ" ++ name ++ " → " ++ show expr
  show (EApp e1 e2) = show e1 ++ "(" ++ show e2 ++ ")"
  show (ELet name value body) = "let " ++ name ++ " = " ++ show value ++ " in " ++ show body
