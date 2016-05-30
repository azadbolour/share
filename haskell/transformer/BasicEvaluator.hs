
module BasicEvaluator where

import Data.Maybe
import qualified Data.Map as Map
import EvaluatorTypes

-- | Naive non-monadic evaluator - ignores errors, uses environment as a parameter.
eval :: Env -> Exp -> Value

eval env (Lit i) = IntVal i

eval env (Var var) =
  fromJust (Map.lookup var env)  -- This will fail if the variable does not exist in the environment.

eval env (Plus expr1 expr2) =
  let
    IntVal val1 = eval env expr1   -- The pattern match is allowed but will fail if the expression evaluates to a function.
    IntVal val2 = eval env expr2
  in IntVal (val1 + val2 )

eval env (Abs var expr) = FunVal env var expr

eval env (App expr1 expr2 ) =
  let
    fun = eval env expr1
    val = eval env expr2
  in case fun of
    FunVal closureEnv var body -> eval (Map.insert var val closureEnv) body
    -- Fails if expr1 does not return a function.

exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))
exampleExpBad = Lit 12 `Plus` (App (Abs "x" (Var "y")) (Lit 4 `Plus` Lit 2))

main :: IO ()

main = do
  print $ eval Map.empty exampleExp
  -- print $ eval Map.empty exampleExpBad
  -- print $ eval Map.empty (App (Lit 5) (Lit 4 `Plus` Lit 2))


