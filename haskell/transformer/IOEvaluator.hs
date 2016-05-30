
module IOEvaluator where

import Control.Monad.Identity
import Control.Monad.Error.Class
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import qualified Data.Map as Map
import EvaluatorTypes

{-
  Include the IO in the evaluation.

  We replace Identity with IO and make necessary adjustments.
-}

type OperationCount = Integer

-- The value we are interested is the expression value augmented with
-- the number of operations executed to evaluate the expression.
type EffectfulVal val = ReaderT Env (ExceptT String (WriterT [String] (StateT OperationCount IO))) val

runEffectfulVal :: Env -> OperationCount -> EffectfulVal val -> IO ((Either String val, [String]), OperationCount)
runEffectfulVal env = \initialCount effectfulVal -> runStateT(runWriterT(runExceptT (runReaderT effectfulVal env))) initialCount

tick :: (Num count, MonadState count stateMonad) => stateMonad ()
tick = do
  count <- get
  put (count + 1)

eval :: Exp -> EffectfulVal Value

eval (Lit i) = do
  liftIO $ print i
  tick
  return $ IntVal i

eval (Var var) = do
  tick
  tell [var]
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
  val1 <- runEffectfulVal Map.empty initialCount (eval exampleExp)
  print val1
  val2 <- runEffectfulVal Map.empty initialCount (eval exampleExpBad)
  print val2
  val3 <- runEffectfulVal Map.empty initialCount (eval (Plus (Lit 1) (Abs "x" (Var "x"))))
  print val3

