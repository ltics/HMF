module BP.Type where

type Id = Int
type TName = String
type TVarInstance = Maybe Type
type TVarName = Maybe String
type TOpTypes = [Type]

data Type = TypeVarriable Id TVarInstance TVarName
          | TypeOperator TName TOpTypes

intT :: Type
intT = TypeOperator "int" []

boolT :: Type
boolT = TypeOperator "bool" []

type FromT = Type
type ToT = Type

functionT :: FromT -> ToT -> Type
functionT fromType toType = TypeOperator "→" [fromType, toType]