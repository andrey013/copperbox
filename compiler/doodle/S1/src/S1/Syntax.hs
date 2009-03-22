{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE UndecidableInstances       #-}


module S1.Syntax where

import Language.KURE
import Control.Monad
import Data.Monoid


-- data Type = TyBool | TyReal | TyPair Ty Ty | TyArrow Ty Ty

type Name = String

data Expr = Unit 
          | Var   Name
          | Bool  Bool
          | Real  Double
          | Fn    Name    Expr
          | App   Expr    Expr
          | If    Expr    Expr    Expr
          | Let   Name    Expr    Expr
          | Bin   Expr    BinOp   Expr
  deriving (Show)

data BinOp = Add | Sub | Mul | Div | Rem 
  deriving (Eq,Show)
  

--------------------------------------------------------------------------------
-- Kure

data SynGeneric = GExpr Expr

                
instance Term Expr where
  type Generic Expr   = SynGeneric
  inject              = GExpr
  select (GExpr expr) = Just expr
  
  
instance Term SynGeneric where
  -- SynGeneric is its own Generic root.
  type Generic SynGeneric = SynGeneric
  inject   = id
  select e = Just e
  
instance (Walker m dec Expr,
          Monad m, Monoid dec) => Walker m dec SynGeneric where
  allR rr = transparently $ rewrite $ \ e -> case e of
          GExpr s -> liftM GExpr $ apply (allR rr) s
  crushU rr = translate $ \ e -> case e of
            GExpr s -> apply (crushU rr) s