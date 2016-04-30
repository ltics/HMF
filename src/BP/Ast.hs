{-# OPTIONS_GHC -Wall #-}

module BP.Ast where

import qualified Text.PrettyPrint as PP

type EName = String

data Term = Ident EName
          | Lambda EName Term
          | Apply Term Term
          | Let EName Term Term
          | LetRec EName Term Term

stringOfTerm :: Term -> String
stringOfTerm t = case t of
                  Ident n -> n
                  Lambda v b -> "λ" ++ v ++ " → " ++ stringOfTerm b
                  Apply fn arg -> stringOfTerm fn ++ "(" ++ stringOfTerm arg ++ ")"
                  Let v def body -> "let " ++ v ++ " = " ++ stringOfTerm def ++ " in " ++ stringOfTerm body
                  LetRec v def body -> "letrec " ++ v ++ " = " ++ stringOfTerm def ++ " in " ++ stringOfTerm body

instance Show Term where
    showsPrec _ x = shows $ PP.text $ stringOfTerm x

