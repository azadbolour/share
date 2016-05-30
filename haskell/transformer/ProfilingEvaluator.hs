
module ProfilingEvaluator where

import Control.Monad.Identity
import Control.Monad.Error.Class
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as Map
import EvaluatorTypes

{-
  Evaluator that has the environment factored out and profiles evaluation operations.
-}

-- See README.md for signatures and descriptions of the transformers.

type OperationCount = Integer

-- The value we are interested is the expression value augmented with
-- the number of operations executed to evaluate the expression.
type EffectfulVal val = ReaderT Env (ExceptT String (StateT OperationCount Identity)) val

runEffectfulVal :: Env -> OperationCount -> EffectfulVal val -> (Either String val, OperationCount)
runEffectfulVal env = \initialCount effectfulVal -> runIdentity(runStateT(runExceptT (runReaderT effectfulVal env)) initialCount)

tick :: (Num count, MonadState count stateMonad) => stateMonad ()
tick = do
  count <- get
  put (count + 1)

eval :: Exp -> EffectfulVal Value

eval (Lit i) = do
  tick
  return $ IntVal i

eval (Var var) = do
  tick
  env <- ask
  case Map.lookup var env of
    Nothing -> throwError ("unbound variable: " ++ var)
    Just val -> return val

eval (Plus expr1 expr2) = do
  tick
  val1 <- eval expr1
  val2 <- eval expr2
  case (val1, val2) of
    (IntVal i1, IntVal i2) ->
      return $ IntVal (i1 + i2)
    _ -> throwError "type error"

eval (Abs var expr) = do
  tick
  env <- ask
  return $ FunVal env var expr

eval (App expr1 expr2) = do
  tick
  fun <- eval expr1
  val <- eval expr2
  case fun of
    FunVal closureEnv name body ->
      local(const(Map.insert name val closureEnv)) (eval body)
    _ -> throwError "type error"

exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))
exampleExpBad = Lit 12 `Plus` (App (Abs "x" (Var "y")) (Lit 4 `Plus` Lit 2))


main :: IO ()

main = do
  let initialCount = 0
  print $ runEffectfulVal Map.empty initialCount (eval exampleExp)
  print $ runEffectfulVal Map.empty initialCount (eval exampleExpBad)
  print $ runEffectfulVal Map.empty initialCount (eval (Plus (Lit 1) (Abs "x" (Var "x"))))

  -- TODO. How can I force evaluation inside the identity monad but get the monadic value and print it out?

