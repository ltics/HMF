{-# OPTIONS_GHC -Wall #-}

module BP.Type where

import State
import Data.IORef
import Data.List(intercalate)
import System.IO.Unsafe(unsafePerformIO)
import qualified Text.PrettyPrint as PP

type Id = Int
type TName = String
type TVarInstance = Maybe Type
type TVarName = Maybe String
type TOpTypes = [Type]

data Type = TypeVariable Id (IORef TVarInstance) (IORef TVarName) -- if instance is Nothing then the type variable is unbound
          | TypeOperator TName TOpTypes

instance Eq Type where
    TypeVariable id1 inst1 vname1 == TypeVariable id2 inst2 vname2 = id1 == id2 && instV1 == instV2 && vnameV1 == vnameV2 where
                                                                          instV1 = readState inst1
                                                                          instV2 = readState inst2
                                                                          vnameV1 = readState vname1
                                                                          vnameV2 = readState vname2
    TypeOperator name1 types1 == TypeOperator name2 types2 = name1 == name2 && types1 == types2
    _ == _ = False

instance Ord Type where
    TypeVariable id1 inst1 vname1 <= TypeVariable id2 inst2 vname2 = id1 <= id2 && instV1 <= instV2 && vnameV1 <= vnameV2 where
                                                                          instV1 = readState inst1
                                                                          instV2 = readState inst2
                                                                          vnameV1 = readState vname1
                                                                          vnameV2 = readState vname2
    TypeOperator name1 types1 <= TypeOperator name2 types2 = name1 <= name2 && types1 <= types2
    _ <= _ = False

instance Show Type where
    showsPrec _ x = shows $ PP.text $ unsafePerformIO $ stringOfType x

stringOfType :: Type -> Infer String
stringOfType (TypeVariable _ inst name) = do
  instV <- readIORef inst
  case instV of
    Just inst' -> stringOfType inst'
    Nothing -> do
      nameV <- readIORef name
      case nameV of
        Just name' -> return name'
        Nothing -> do
          newVarName <- nextUniqueName
          writeIORef name $ Just newVarName
          return newVarName
stringOfType (TypeOperator name types) = case length types of
                                          0 -> return name
                                          2 -> return $ "(" ++ unwords [show (types!!0), name, show (types!!1)] ++ ")"
                                          _ -> return $ name ++ "[" ++ intercalate "," (map show types) ++ "]"

intT :: Type
intT = TypeOperator "int" []

boolT :: Type
boolT = TypeOperator "bool" []

type FromT = Type
type ToT = Type

functionT :: FromT -> ToT -> Type
functionT fromType toType = TypeOperator "→" [fromType, toType]
