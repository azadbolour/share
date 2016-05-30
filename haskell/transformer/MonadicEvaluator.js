
module Evaluators where

import Control.Monad.Identity
import Data.Maybe
import qualified Data.Map as Map
import EvaluatorTypes

{-
  Evaluate expression inside a generic monad.
  Delay the choice of the actual monad to use to the call site by using a getter (runner)
  of the right type to extract the embedded computed value. For example, to force the
  identity monad to be used, extract the computed value by using runIdentity.
-}

-- | Monadic evaluator - generic in the monad's data type.
evalMonadic :: Monad m => Env -> Exp -> m Value

evalMonadic env (Lit i) = return $ IntVal i

evalMonadic env (Var var) = maybe (fail ("undefined variable: " ++ var)) return $ Map.lookup var env

evalMonadic env (Plus expr1 expr2) = do
  IntVal val1 <- evalMonadic env expr1
  IntVal val2 <- evalMonadic env expr2
  return $ IntVal (val1 + val2)

evalMonadic env (Abs var expr) = return $ FunVal env var expr

evalMonadic env (App expr1 expr2) = do
  fun <- evalMonadic env expr1
  val <- evalMonadic env expr2
  case fun of
    FunVal closureEnv name body ->
      evalMonadic (Map.insert name val closureEnv) body

exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))
exampleExpBad = Lit 12 `Plus` (App (Abs "x" (Var "y")) (Lit 4 `Plus` Lit 2))

main :: IO ()

main = do
  print $ runIdentity $ evalMonadic Map.empty exampleExp  
  -- runIdentity forces the evaluation to occur in the identity monad


