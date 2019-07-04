{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module AST (
  Expr(..),
  Lit(..),
  TType(..),
  Error(..),
  Idx(..),
  typecheck
               ) where

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Map             as Map

-- TODO add support for following (derived where possible)
-- fix (can't be typed if defined in pure lambda calculus so must be built-in)
-- pattern matching
-- n-tuples
-- records
-- sum types
-- string type
-- float type
-- other types?

-- Use De Bruijn indices to avoid a whole host of alpha substitution problems
newtype Idx = Idx { getIdx :: Int } deriving (Eq, Ord, Num, Show)

data Expr = Lit Lit
          | Var Idx
          | Lam Expr TType Expr
          | App Expr Expr
          | If Expr Expr Expr
          | Let Expr TType Expr Expr -- can't be represented by Lam/App in typed lambda calculus (without type inference)
          deriving (Eq, Show)

data Lit = ETrue
         | EFalse
         | EInt Int
         deriving (Eq, Show)

data TType = TBool
           | TInt
           | TFn TType TType
           deriving (Eq, Show)

data Error = TypeError
           | UnboundVariableError Idx
           | UnreachableError
           deriving (Show)

newtype Env = Env { getEnv :: Map.Map Idx TType }

emptyEnv :: Env
emptyEnv = Env Map.empty

extendEnv :: Idx -> TType -> Env -> Env
extendEnv idx tpe = Env . Map.insert idx tpe . getEnv

typecheck :: Expr -> Either Error TType
typecheck = runExcept . flip runReaderT emptyEnv . typecheck_

typecheck_ :: (MonadError Error m, MonadReader Env m) => Expr -> m TType
typecheck_ (Lit ETrue)         = return TBool
typecheck_ (Lit EFalse)        = return TBool
typecheck_ (Lit (EInt _))      = return TInt
typecheck_ (Var idx) = do
  tpe <- reader (Map.lookup idx . getEnv)
  maybe (throwError TypeError) return tpe
typecheck_ (Lam (Var idx) tpe e) = TFn tpe <$> local (extendEnv idx tpe) (typecheck_ e)
typecheck_ (App e1 e2) = do
  t1 <- typecheck_ e1
  t2 <- typecheck_ e2
  case t1 of
    TFn from to -> if (from == t2) then return to else throwError TypeError
    _           -> throwError TypeError
typecheck_ (If cond tru fls) = do
  t1 <- typecheck_ cond
  t2 <- typecheck_ tru
  t3 <- typecheck_ fls
  if (t1 /= TBool || t2 /= t3) then throwError TypeError else return ()
  return t2
typecheck_ (Let (Var idx) tpe _ e) = local (extendEnv idx tpe) (typecheck_ e)
typecheck_  _                   = throwError TypeError
