
module EvaluatorTypes
  (
    VarName,
    Exp(..),
    Value(..),
    Env
  ) where

import qualified Data.Map as Map

{-
  Basic data types for expression evaluator.
-}

-- | Variable names.
type VarName = String -- variable names

-- | Expressions.
data Exp =
    Lit Integer
  | Var VarName
  | Plus Exp Exp
  | Abs VarName Exp
  | App Exp Exp
    deriving (Show)

-- | Values.
data Value =
    IntVal Integer
  | FunVal Env VarName Exp
    deriving (Show)

-- | Evaluation environment.
type Env = Map.Map VarName Value

