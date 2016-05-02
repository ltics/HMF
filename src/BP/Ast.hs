{-# OPTIONS_GHC -Wall #-}

module BP.Ast where

import BP.Type
import Data.List(intercalate)
import qualified Text.PrettyPrint as PP

type EName = String
type Terms = [Term]
data ParamWithOptionalType = Param EName (Maybe Type)

data Term = Ident EName
          | Lambda EName Term
          | Function EName [ParamWithOptionalType] Term (Maybe Type)
          | Apply Term Term
          | Call Term Terms
          | Let EName Term Term
          | LetBinding EName Term (Maybe Type)
          | LetRec EName Term Term

stringOfParam :: ParamWithOptionalType -> String
stringOfParam (Param name t) = case t of
                                Just t' -> name ++ " : " ++ show t'
                                Nothing -> name

stringOfTerm :: Term -> String
stringOfTerm t = case t of
                  Ident n -> n
                  Lambda v b -> "λ" ++ v ++ " → " ++ stringOfTerm b
                  --Function
                  Function name params body rtnType -> "ƒ " ++ name ++ "(" ++ intercalate ", " (map stringOfParam params) ++ ") → "
                                                      ++ stringOfTerm body ++ case rtnType of
                                                                                Just t' -> " : " ++ show t'
                                                                                Nothing -> ""
                  Apply fn arg -> "(" ++ stringOfTerm fn ++ " " ++ stringOfTerm arg ++ ")"
                  Call fn args -> "(" ++ stringOfTerm fn ++ intercalate ", " (map stringOfTerm args) ++ ")"
                  Let v def body -> "let " ++ v ++ " = " ++ stringOfTerm def ++ " in " ++ stringOfTerm body
                  LetBinding v def ty -> "let " ++ v ++ stringOfTerm def ++ case ty of
                                                                            Just ty' -> " : " ++ show ty'
                                                                            Nothing -> ""
                  LetRec v def body -> "letrec " ++ v ++ " = " ++ stringOfTerm def ++ " in " ++ stringOfTerm body

instance Show Term where
    showsPrec _ x = shows $ PP.text $ stringOfTerm x

