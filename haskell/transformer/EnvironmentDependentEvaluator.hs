
module EnvironmentDependentEvaluator where

import Control.Monad.Identity
import Control.Monad.Error.Class
import Control.Monad.Except
import Control.Monad.Reader
import Data.Maybe
import qualified Data.Map as Map
import EvaluatorTypes

{-
  Evaluator that has the environment factored out.
-}

-- See README.md for signatures and descriptions of the transformers.

-- We factor out the environment and make our effectful value depend on it.
-- The effectful value does not include the environment but encapsulates a function of the environment.
type EffectfulVal val = ReaderT Env (ExceptT String Identity) val

-- So now we have to close on the environment (pass in the environment)
-- to get an extractor function for the embedded expression value as before.
runEffectfulVal :: Env -> EffectfulVal val -> Either String val
runEffectfulVal env = \effectfulVal -> (runIdentity (runExceptT (runReaderT effectfulVal env)))

-- The environment has been factored out of the evaluator.
-- It will be passed in at the end of the proceess to get the expression value.
eval :: Exp -> EffectfulVal Value

eval (Lit i) = return $ IntVal i

eval (Var var) = do
  env <- ask
  case Map.lookup var env of
    Nothing -> throwError ("unbound variable: " ++ var)
    Just val -> return val

eval (Plus expr1 expr2) = do
  val1 <- eval expr1
  val2 <- eval expr2
  case (val1, val2) of
    (IntVal i1, IntVal i2) ->
      return $ IntVal (i1 + i2)
    _ -> throwError "type error"

eval (Abs var expr) = do
  env <- ask
  return $ FunVal env var expr

eval (App expr1 expr2) = do
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
  print $ runEffectfulVal Map.empty (eval exampleExp)
  print $ runEffectfulVal Map.empty (eval exampleExpBad)
  print $ runEffectfulVal Map.empty (eval (Plus (Lit 1) (Abs "x" (Var "x"))))

  -- TODO. How can I force evaluation inside the identity monad but get the monadic value and print it out?

