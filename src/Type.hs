{-# OPTIONS_GHC -Wall #-}

module Type where

import State
import Data.List (intercalate, sort)
import Data.IORef
import System.IO.Unsafe(unsafePerformIO)
import Control.DeepSeq
import qualified Data.Map as M
import qualified Text.PrettyPrint as PP

type Name = String
type Id = Int -- type variable identifier
type Rank = Int -- type variable rank stand for position in Γ

data T = TConst Name -- type constants int, bool, string
       | TArrow [T] T
       | TApp T [T]
       | TVar (IORef TV)

data TV = Unbound Id Rank
        | Link T
        | Generic Id
        deriving (Eq)

instance Eq T where
    TConst name1 == TConst name2 = name1 == name2
    TArrow params1 rtn1 == TArrow params2 rtn2 = params1 == params2 && rtn1 == rtn2
    TApp fn1 args1 == TApp fn2 args2 = fn1 == fn2 && args1 == args2
    TVar var1 == TVar var2 = let a = readState var1 in
                                let b = readState var2 in
                                    a == b
    _ == _ = False

instance Show T where
    showsPrec _ x = shows $ PP.text $ prType x

nameOfInt :: Int -> String
nameOfInt i = let name = [(toEnum $ 97 + i `mod` 26) :: Char]
              in if i >= 26 then name ++ show (i `quot` 26) else name

idNameMap :: IORef (M.Map Int String)
idNameMap = createState M.empty

count :: IORef Int
count = createState 0

idNameMapReset :: Infer ()
idNameMapReset = writeIORef idNameMap M.empty

countReset :: Infer ()
countReset = writeIORef count 0

prType' :: T -> String
prType' t =
    case t of
         TConst name -> name
         TArrow params rtn -> case params of
                                    [param] -> case param of
                                                   (TArrow _ _) -> "(" ++ prType' param ++ ")" ++ " → " ++ prType' rtn
                                                   _ -> prType' param ++ " → " ++ prType' rtn
                                    _ -> "(" ++ intercalate ", " (map prType' params) ++ ")" ++ " → " ++ prType' rtn
         TApp fn args -> prType' fn ++ "[" ++ intercalate ", " (map prType' args) ++ "]"
         TVar var -> case readState var of
                           Unbound tvId _ -> "_" ++ show tvId
                           Link t' -> prType' t'
                           Generic tvId -> let m = readState idNameMap in
                                                           case M.lookup tvId m of
                                                           Just name -> name
                                                           Nothing -> unsafePerformIO $ do
                                                            c <- readIORef count
                                                            let name = nameOfInt c
                                                            modifyIORef idNameMap (\m' -> M.insert tvId name m')
                                                            modifyIORef count (+1)
                                                            return name

prType :: T -> String
prType t = unsafePerformIO $ do
    let lit = prType' t
    c <- lit `deepseq` readIORef count
    idNames <- readIORef idNameMap
    let lit' = if c > 0
               then "∀" ++ intercalate "," (sort $ M.elems idNames) ++ ". " ++ lit
               else lit
    idNameMapReset
    countReset
    return lit'
