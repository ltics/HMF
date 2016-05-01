{-# OPTIONS_GHC -Wall #-}

module BP.Ast where

import BP.Type
import Data.List(intercalate)
import qualified Text.PrettyPrint as PP

type EName = String
type Terms = [Term]
data ParamWithOptionalType = ParamWithOptionalType EName (Maybe Type)

data Term = Ident EName
          | Lambda EName Term
          | Function [ParamWithOptionalType] Term (Maybe Type)
          | Apply Term Term
          | Call Term Terms
          | Let EName Term Term
          | LetRec EName Term Term

stringOfParam :: ParamWithOptionalType -> String
stringOfParam (ParamWithOptionalType name t) = case t of
                                                Just t' -> name ++ " : " ++ show t'
                                                Nothing -> name

stringOfTerm :: Term -> String
stringOfTerm t = case t of
                  Ident n -> n
                  Lambda v b -> "λ" ++ v ++ " → " ++ stringOfTerm b
                  --Function
                  Function params body rtnType -> "ƒ(" ++ intercalate ", " (map stringOfParam params) ++ ") → " ++ case rtnType of
                                                                                                                    Just t' -> stringOfTerm body ++ " : " ++ show t'
                                                                                                                    Nothing -> stringOfTerm body
                  Apply fn arg -> "(" ++ stringOfTerm fn ++ " " ++ stringOfTerm arg ++ ")"
                  Call fn args -> "(" ++ stringOfTerm fn ++ intercalate ", " (map stringOfTerm args) ++ ")"
                  Let v def body -> "let " ++ v ++ " = " ++ stringOfTerm def ++ " in " ++ stringOfTerm body
                  LetRec v def body -> "letrec " ++ v ++ " = " ++ stringOfTerm def ++ " in " ++ stringOfTerm body

instance Show Term where
    showsPrec _ x = shows $ PP.text $ stringOfTerm x

