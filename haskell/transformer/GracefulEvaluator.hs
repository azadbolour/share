
module GracefulEvaluator where

import Control.Monad.Identity
import Control.Monad.Error.Class
import Control.Monad.Except
import Data.Maybe
import qualified Data.Map as Map
import EvaluatorTypes

{-
  Evaluator that fails gracefully.

  Reminder - Types of functions used in failure processing.

    maybe :: b -> (a -> b) -> Maybe a -> b
        applies the function to the Just value or uses a provided default value for the function
    fail :: String -> m a
        fails with a message - has the right monadic type
-}

{-
   Embed a Just value of a Maybe in another monad
   or fail in that other monad if the Maybe is Nothing.
-}

returnValueOrFail :: Monad m => Maybe a -> String -> m a
returnValueOrFail maybeValue message = maybe (fail message) return $ maybeValue

-- See README for transformer signatures and descriptions.

type EffectfulVal val = ExceptT String Identity val

runEffectfulVal :: EffectfulVal val -> Either String val
runEffectfulVal = runIdentity . runExceptT

-- TODO. Get type matching errors with the generic formulation of eval. Decipher error message.
-- eval :: Monad m => Env -> Exp -> m Value
eval :: Env -> Exp -> EffectfulVal Value

eval env (Lit i) = return $ IntVal i

eval env (Var var) =
  let
      maybeValue = Map.lookup var env
      message = "undefined variable: " ++ var
  in returnValueOrFail maybeValue message

eval env (Plus expr1 expr2) = do
  val1 <- eval env expr1
  val2 <- eval env expr2
  case (val1, val2) of
    (IntVal i1, IntVal i2) ->
      return $ IntVal (i1 + i2)
    _ -> throwError "type error"

eval env (Abs var expr) = return $ FunVal env var expr

eval env (App expr1 expr2) = do
  fun <- eval env expr1
  val <- eval env expr2
  case fun of
    FunVal closureEnv name body ->
      eval (Map.insert name val closureEnv) body
    _ -> throwError "type error"

exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))
exampleExpBad = Lit 12 `Plus` (App (Abs "x" (Var "y")) (Lit 4 `Plus` Lit 2))


main :: IO ()

main = do
  print $ runEffectfulVal $ eval Map.empty exampleExp
  print $ (runEffectfulVal $ eval Map.empty exampleExpBad)
  print $ runEffectfulVal $ eval Map.empty (Plus (Lit 1) (Abs "x" (Var "x")))

  -- TODO. How can I force evaluation inside the identity monad but get the monadic value and print it out?

