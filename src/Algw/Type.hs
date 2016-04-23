{-# OPTIONS_GHC -Wall #-}

module Algw.Type where

type TName = String

data T = TInt
       | TBool -- these two can also use TConst to make it more extendable
       | TVar TName
       | TArrow T T
  deriving (Eq, Ord)

instance Show T where
  show TInt = "int"
  show TBool = "bool"
  show (TVar name) = name
  show (TArrow t1@(TArrow _ _) t2) = "(" ++ show t1 ++ ")" ++ " → " ++ show t2
  show (TArrow t1 t2) = show t1 ++ " → " ++ show t2

data Scheme = Mono T
            | Poly TName Scheme
  deriving (Eq, Ord)