{-# LANGUAGE FlexibleContexts #-}
module Eval (
  eval
            ) where

import           AST
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Map             as Map

newtype Env = Env { getEnv :: Map.Map Idx Expr } deriving (Show)

emptyEnv :: Env
emptyEnv = Env Map.empty

extendEnv :: Idx -> Expr -> Env -> Env
extendEnv idx tpe = Env . Map.insert idx tpe . getEnv

-- Beta-reduction
eval :: Expr -> Either Error Expr
eval e = typecheck e >> (runExcept . flip runReaderT emptyEnv $ eval_ e)

-- We assume that the expression is well-typed
-- TODO check that env does not already have a binding for idx?
-- TODO need relabelling and to actually use De Bruijn indices?
eval_ :: (MonadReader Env m, MonadError Error m) => Expr -> m Expr
eval_ e@(Lit _) = return e
eval_ (NumOp e) = evalNumericBuiltin e
eval_ (Var idx) = do
  e <- reader (Map.lookup idx . getEnv)
  maybe (throwError $ UnboundVariableError idx) return e
eval_ e@(Lam _ _ _) = return e
eval_ (App (Lam (Var idx) _ e1) e2) = do
  arg <- eval_ e2
  local (extendEnv idx arg) (eval_ e1)
eval_ (If cond e1 e2) = do
  c <- eval_ cond
  case c of
    (Lit ETrue)  -> eval_ e1
    (Lit EFalse) -> eval_ e2
    _            -> throwError UnreachableError
eval_ (Let (Var idx) _ e1 e2) = do
  bind <- eval_ e1
  local (extendEnv idx bind) (eval_ e2)
eval_ (Proj name (ERecord fields)) = maybe (throwError UnreachableError) (return . Lit) (Map.lookup name fields)
eval_ _ = throwError UnreachableError

--TODO complete this
evalNumericBuiltin :: (MonadReader Env m, MonadError Error m) => NumericBuiltin -> m Expr
evalNumericBuiltin (Plus e1 e2) = do
  v1 <- eval_ e1
  v2 <- eval_ e2
  case (v1, v2) of
    (Lit (EInt n), Lit (EInt m)) -> return . Lit $ EInt (m + n)
    _ -> throwError UnreachableError