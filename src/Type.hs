{-# OPTIONS_GHC -Wall #-}

module Type where

import Data.List (intercalate, sort)
import qualified Data.Map as M
import Data.IORef
import System.IO.Unsafe(unsafePerformIO)
import Control.DeepSeq

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

instance Show T where
    showsPrec _ x = shows (prType' x)

nameOfInt :: Int -> String
nameOfInt i = let name = [(toEnum $ 97 + i `mod` 26) :: Char]
              in if i >= 26 then name ++ show (i `quot` 26) else name

idNameMap :: IORef (M.Map Int String)
idNameMap = unsafePerformIO $ newIORef M.empty

count :: IORef Int
count = unsafePerformIO $ newIORef 0

readState :: IORef c -> c
readState = unsafePerformIO . readIORef

prType :: T -> String
prType t =
    case t of
         (TConst name) -> name
         (TArrow params rtn) -> case params of
                                    [param] -> case param of
                                                   (TArrow _ _) -> "(" ++ prType param ++ ")" ++ " → " ++ prType rtn
                                                   _ -> prType param ++ " → " ++ prType rtn
                                    _ -> "(" ++ intercalate ", " (map prType params) ++ ")" ++ " → " ++ prType rtn
         (TApp ty tys) -> prType ty ++ "[" ++ intercalate ", " (map prType tys) ++ "]"
         (TVar var) -> case readState var of
                           (Unbound tvId _) -> "_" ++ show tvId
                           (Link t') -> prType t'
                           (Generic tvId) -> let m = readState idNameMap in
                                                           case M.lookup tvId m of
                                                           (Just name) -> name
                                                           Nothing -> unsafePerformIO $ do
                                                            c <- readIORef count
                                                            let name = nameOfInt c
                                                            _ <- modifyIORef idNameMap (\m' -> M.insert tvId name m')
                                                            _ <- modifyIORef count (+1)
                                                            return name

prType' :: T -> String
prType' t = unsafePerformIO $ do
    let lit = prType t
    let lit' = lit `deepseq` if readState count > 0 then let names = M.elems $ readState idNameMap in
                                                          "∀" ++ intercalate "," (sort names) ++ ". " ++ lit
             else lit
    _ <- writeIORef idNameMap M.empty
    _ <- writeIORef count 0
    return lit'

