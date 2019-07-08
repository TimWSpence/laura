{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

module AST (
  Expr(..),
  Lit(..),
  TType(..),
  Error(..),
  Idx(..),
  NumericBuiltin(..),
  typecheck
               ) where

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Map             as Map
import           Data.Traversable

-- TODO add support for following (derived where possible)
-- fix (can't be typed if defined in pure lambda calculus so must be built-in)
-- pattern matching
-- n-tuples
-- records
-- sum types
-- string type
-- float type
-- parametric polymorphism
-- subtyping
-- other types?

-- Might use De Bruijn indices to avoid a whole host of alpha substitution problems
newtype Idx = Idx { getIdx :: Int } deriving (Eq, Ord, Num, Show)

type Field = String

-- TODO separate concept of values and expressions?
data Expr = Lit Lit
          | Var Idx
          | Lam Expr TType Expr
          | App Expr Expr
          | If Expr Expr Expr
          | Let Expr TType Expr Expr -- can't be represented by Lam/App in typed lambda calculus (without type inference)
          | NumOp NumericBuiltin
          | Proj Field Lit
          deriving (Eq, Show)

data Lit = ETrue
         | EFalse
         | EInt Int
         | EFloat Float
         | ERecord (Map.Map Field Lit)
         deriving (Eq, Show)

data NumericBuiltin = Plus Expr Expr
                    | Minus Expr Expr
                    | Neg Expr
                    | Times Expr Expr
                    | Div Expr Expr
                    | Mod Expr Expr
                    deriving (Eq, Show)

data TType = TBool
           | TInt
           | TFloat
           | TRecord (Map.Map Field TType)
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
typecheck_ (Lit (EFloat _))    = return TFloat
typecheck_ (Lit (ERecord fields))    = TRecord <$> Map.traverseWithKey (\_ v -> typecheck_ (Lit v)) fields
typecheck_ (NumOp e) = typecheckNumericBuiltin e
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
typecheck_ (Proj name e@(ERecord fields)) = do
  t <- typecheck_ (Lit e)
  case t of
    (TRecord tpes) -> maybe (throwError TypeError) return (Map.lookup name tpes)
    _ -> throwError TypeError
typecheck_  _                   = throwError TypeError

-- I don't believe we have a better way to do this without going down the System F route to support polymorphism
typecheckNumericBuiltin :: forall m. (MonadError Error m, MonadReader Env m) => NumericBuiltin -> m TType
typecheckNumericBuiltin e = case e of
  (Plus e1 e2)  -> checkInfix e1 e2
  (Minus e1 e2) -> checkInfix e1 e2
  (Times e1 e2) -> checkInfix e1 e2
  (Div e1 e2)   -> checkInfix e1 e2
  (Mod e1 e2)   -> checkInfix e1 e2
  (Neg e1)      -> checkPrefix e1
  where
    checkInfix :: Expr -> Expr -> m TType
    checkInfix e1 e2 = do
      t1 <- typecheck_ @m e1
      t2 <- typecheck_ e2
      case (t1, t2) of
        (TInt, TInt)     -> return TInt
        (TInt, TFloat)   -> return TFloat
        (TFloat, TInt)   -> return TFloat
        (TFloat, TFloat) -> return TFloat
        _                -> throwError TypeError

    checkPrefix :: Expr -> m TType
    checkPrefix e = do
        t <- typecheck_ e
        case t of
          TInt   -> return TInt
          TFloat -> return TFloat
          _      -> throwError TypeError
