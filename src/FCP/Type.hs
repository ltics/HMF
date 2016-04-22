{-# OPTIONS_GHC -Wall #-}

module FCP.Type where

import State
import Data.List (intercalate)
import Data.IORef
import System.IO.Unsafe(unsafePerformIO)
import Control.Monad.Loops (allM)
import qualified Data.Map as M
import qualified Text.PrettyPrint as PP

type Name = String
type Id = Int -- type variable identifier
type Rank = Int -- type variable rank stand for position in Γ
type IdName = M.Map Id Name

data T = TConst Name -- type constants int, bool, string
       | TArrow [T] T
       | TApp T [T]
       | TVar (IORef TV)
       | TForall [Id] T -- polymorphic type ∀a,b. a → b

data TV = Unbound Id Rank
        | Bound Id
        | Link T
        | Generic Id
  deriving (Eq, Ord)

unlink :: T -> Infer T
unlink t = case t of
            TVar var -> do
                varV <- readIORef var
                case varV of
                    Link t' -> do
                        t'' <- unlink t'
                        writeIORef var $ Link t''
                        return t''
                    _ -> return t
            _ -> return t

isMonomorphic :: T -> Infer Bool
isMonomorphic (TConst _) = return True
isMonomorphic (TArrow params rtn) = do
    paramsPred <- allM isMonomorphic params
    rtnPred <- isMonomorphic rtn
    return $ paramsPred && rtnPred
isMonomorphic (TApp fn args) = do
    fnPred <- isMonomorphic fn
    argsPred <- allM isMonomorphic args
    return $ fnPred && argsPred
isMonomorphic (TVar var) = do
    varV <- readIORef var
    case varV of
        Link t -> isMonomorphic t
        _ -> return True
isMonomorphic (TForall _ _) = return False

instance Eq T where
    TConst name1 == TConst name2 = name1 == name2
    TArrow params1 rtn1 == TArrow params2 rtn2 = params1 == params2 && rtn1 == rtn2
    TApp fn1 args1 == TApp fn2 args2 = fn1 == fn2 && args1 == args2
    TForall ids1 t1 == TForall ids2 t2 = ids1 == ids2 && t1 == t2
    TVar var1 == TVar var2 = let a = readState var1 in
                                let b = readState var2 in
                                    a == b
    _ == _ = False

instance Ord T where
    TConst name1 <= TConst name2 = name1 <= name2
    TArrow params1 rtn1 <= TArrow params2 rtn2 = params1 <= params2 && rtn1 <= rtn2
    TApp fn1 args1 <= TApp fn2 args2 = fn1 <= fn2 && args1 <= args2
    TForall ids1 t1 <= TForall ids2 t2 = ids1 <= ids2 && t1 <= t2
    TVar var1 <= TVar var2 = let a = readState var1 in
                                let b = readState var2 in
                                    a <= b
    _ <= _ = False

instance Show T where
    showsPrec _ x = shows $ PP.text $ stringOfType x

nameOfInt :: Int -> Name
nameOfInt i = let name = [(toEnum $ 97 + i `mod` 26) :: Char]
              in if i >= 26 then name ++ show (i `quot` 26) else name

extendIdNameMap :: IdName -> [Id] -> ([Name], IdName)
extendIdNameMap idNameMap varIds =
    let (nameListRev, idNameMap') = foldl (\(nameList, idNameMap'') varId ->
                                            let newName = nameOfInt (length idNameMap'') in (newName : nameList, M.insert varId newName idNameMap''))
                                         ([], idNameMap)
                                         varIds
    in (reverse nameListRev, idNameMap')

complex :: IdName -> T -> Infer String
complex idNameMap (TArrow params rtn) = do
    paramsStr <- case params of
                    [param] -> simple idNameMap param
                    _ -> do
                        paramsV <- mapM (complex idNameMap) params
                        return $ "(" ++ intercalate ", " paramsV ++ ")"
    rtnStr <- complex idNameMap rtn
    return $ paramsStr ++ " → " ++ rtnStr
complex idNameMap (TForall varIds t) = do
    let (nameList, idNameMap') = extendIdNameMap idNameMap varIds
        nameListStr = intercalate "," nameList
    tV <- complex idNameMap' t
    return $ "∀" ++ nameListStr ++ ". " ++ tV
complex idNameMap t@(TVar var) = do
    varV <- readIORef var
    case varV of
        Link t' -> complex idNameMap t'
        _ -> simple idNameMap t
complex idNameMap t = simple idNameMap t

simple :: IdName -> T -> Infer String
simple _ (TConst name) = return name
simple idNameMap (TApp fn args) = do
    fnStr <- simple idNameMap fn
    argsV <- mapM (complex idNameMap) args
    return $ fnStr ++ "[" ++ intercalate ", " argsV ++ "]"
simple idNameMap (TVar var) = do
    varV <- readIORef var
    case varV of
        Unbound i _ -> return $ "@unknown" ++ show i
        Bound i -> case M.lookup i idNameMap of
                    Just name -> return name
                    Nothing -> error "can not find bound type variable"
        Generic i -> return $ "@generic" ++ show i
        Link t -> simple idNameMap t
simple idNameMap t = do
    tStr <- complex idNameMap t
    return $ "(" ++ tStr ++ ")"

stringOfTypeWithUnboundTypeVars :: IdName -> T -> Infer String
stringOfTypeWithUnboundTypeVars = complex

stringOfType :: T -> String
stringOfType t = unsafePerformIO $ stringOfTypeWithUnboundTypeVars M.empty t
