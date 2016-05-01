module BP.Env where

import BP.Ast
import BP.Type
import qualified Data.Map as M

type Env = M.Map EName Type
