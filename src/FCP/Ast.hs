{-# OPTIONS_GHC -Wall #-}

module FCP.Ast where

import FCP.Type (Id, T, extendIdNameMap, stringOfTypeWithUnboundTypeVars)
import State
import System.IO.Unsafe(unsafePerformIO)
import Data.List (intercalate)
import qualified Data.Map as M
import qualified Text.PrettyPrint as PP

type Name = String
data TAnn = TAnn [Id] T -- type annotation ∃a,b. a → b | ∀a,b. a → b etc.

data EParam = EParam Name (Maybe TAnn)

data Expr = EVar Name
          | EFun [EParam] Expr
          | ECall Expr [Expr]
          | ELet Name Expr Expr
          | EAnn Expr TAnn -- expression with type annotation ƒ : a → b

isAnnotated :: Expr -> Bool
isAnnotated (EAnn _ _) = True
isAnnotated (ELet _ _ body) = isAnnotated body
isAnnotated _ = False

stringOfTypeAnn :: TAnn -> Infer String
stringOfTypeAnn (TAnn varIds t) = do
    let (nameList, idNameMap) = extendIdNameMap M.empty varIds
    tStr <- stringOfTypeWithUnboundTypeVars idNameMap t
    case nameList of
        [] -> return tStr
        _ -> return $ "∃" ++ intercalate "," nameList ++ ". " ++ tStr

instance Show Expr where
    showsPrec _ x = shows $ PP.text $ stringOfExpr x

complex :: Expr -> Infer String
complex (EFun params bodyExpr) = do
    paramsV <- mapM (\(EParam paramName tyAnn) -> case tyAnn of
                                            Just ann -> do
                                                annV <- stringOfTypeAnn ann
                                                return $ "(" ++ paramName ++ " : " ++ annV ++ ")"
                                            Nothing -> return paramName)
                    params
    let paramsStr = intercalate " " paramsV
    rtnStr <- complex bodyExpr
    return $ "ƒ " ++ paramsStr ++ " → " ++ rtnStr
complex (ELet varName valueExpr bodyExpr) = do
    valueV <- complex valueExpr
    bodyV <- complex bodyExpr
    return $ "let " ++ varName ++ " = " ++ valueV ++ " in " ++ bodyV
complex (EAnn expr ann) = do
    exprV <- simple expr
    annV <- stringOfTypeAnn ann
    return $ exprV ++ " : " ++ annV
complex expr = simple expr

simple :: Expr -> Infer String
simple (EVar name) = return name
simple (ECall fnExpr args) = do
    fnV <- simple fnExpr
    argsV <- mapM complex args
    let argsStr = intercalate ", " argsV
    return $ fnV ++ "(" ++ argsStr ++ ")"
simple expr = do
    exprV <- complex expr
    return $ "(" ++ exprV ++ ")"

stringOfExpr :: Expr -> String
stringOfExpr = unsafePerformIO . complex
