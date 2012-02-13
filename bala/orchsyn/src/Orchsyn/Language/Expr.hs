{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Orchsyn.Language.Expr
-- Copyright   :  (c) Stephen Tetley 2012
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  GHC (Generalized newtype deriving)
--
-- Rate types and expressions.
--
--------------------------------------------------------------------------------


module Orchsyn.Language.Expr
  (

    IInit
  , KRate
  , ARate

  , IR 
  , KR 
  , AR

  , DataRate(..)

  , Rate        -- opaque
  , dataRate

  , KA_Rate
  , IK_Rate

  -- * Expressions
  , Var(..)
  , varname

  , Expr
  , uniRate
  , rateOf

  , iexpr
  , kexpr
  , aexpr

  , liftE1
  , liftE2

  , DExpr(..)

  , LVal(..)
  , rval

  ) where

import Orchsyn.Utils.PrettyExpr
import Orchsyn.Utils.PrettyExtras


import Data.Generics	( Data, Typeable )
import Text.PrettyPrint.HughesPJ


data IInit
data KRate
data ARate


type IR = Expr IInit
type KR = Expr KRate
type AR = Expr ARate


-- | Data rate - strictly speaking @I@ is a initialization /time/
-- rather than a rate.
--
data DataRate = I | K | A 
  deriving (Bounded,Enum,Eq,Ord,Show, Data, Typeable)


-- Note - the classes are exported opaquely. This is because there
-- is a fixed, closed set of instances.


class Rate rate where
  dataRate :: Expr rate -> DataRate
  
instance Rate IInit where
  dataRate _ = I

instance Rate KRate where
  dataRate _ = K

instance Rate ARate where
  dataRate _ = A



class Rate rate => KA_Rate rate

instance KA_Rate KRate
instance KA_Rate ARate


class Rate rate => IK_Rate rate

instance IK_Rate IInit
instance IK_Rate KRate

--------------------------------------------------------------------------------
-- Expressions

data Var = Var DataRate Int
         | INamed String                -- I rate (init) only
  deriving (Eq, Ord, Show, Data, Typeable)

varname :: Var -> String
varname (INamed ss)  = ss
varname (Var r i)    = case r of
    I -> 'i' : show i
    K -> 'k' : show i
    A -> 'a' : show i


newtype Expr rate = Expr { getExpr :: DExpr }
  deriving (Eq, Show)


-- | Coerce to the untyped /universal/ rate.
--
-- (Extract an untyped Expr from a typed Expr).
-- 
uniRate :: Rate rate => Expr rate -> DExpr
uniRate = getExpr

-- | Type-level extraction of rate.
-- 
-- @rateOf@ does not need to inspect its argument.
--
rateOf :: Expr rate -> rate
rateOf _e1 = undefined


iexpr :: DExpr -> Expr IInit
iexpr = Expr

kexpr :: DExpr -> Expr KRate
kexpr = Expr

aexpr :: DExpr -> Expr ARate
aexpr = Expr


-- | String data is used by @soundin@ for example.
--
data DExpr = VarE Var
           | FloatE  Double
           | StringE String    
           | IntE    Int
           | PfieldE Int
           | BinE Rator DExpr DExpr
           | UnaryE  Rator DExpr 
           | FunCallE String DExpr
           | CondE DExpr DExpr DExpr
  deriving (Eq, Ord, Show, Data, Typeable)




liftE1 :: (DExpr -> DExpr) -> Expr rate -> Expr rate
liftE1 f = Expr . f . getExpr

liftE2 :: (DExpr -> DExpr -> DExpr) -> Expr rate -> Expr rate -> Expr rate
liftE2 f a b = Expr $ f (getExpr a) (getExpr b)

instance Num (Expr rate) where
  (+) = liftE2 (+)
  (-) = liftE2 (-)
  (*) = liftE2 (*)
  abs = liftE1 abs
  negate   = liftE1 negate
  signum _ = error "signum - no interpretation of signum in Csound."
  fromInteger = Expr . fromInteger

instance Num DExpr where
  (+)     = BinE plus_op
  (-)     = BinE minus_op
  (*)     = BinE mult_op
  abs     = FunCallE "abs"
  negate  = UnaryE unary_negate
  signum _      = error "signum - no interpretation of signum in Csound."
  fromInteger i = FloatE $ fromInteger i


instance Fractional (Expr rate) where
  (/) = liftE2 (/)
  recip _ = error "recip - no interpretation of recip in Csound."  
  fromRational = Expr . fromRational

instance Fractional DExpr where
  (/)     = BinE divide_op
  recip _ = error "recip - no interpretation of recip in Csound."  
  fromRational d = FloatE $ fromRational d


instance Format (Expr rate) where
  format = format . getExpr

instance Format DExpr where
  format = unparse . buildExpr


buildExpr :: DExpr -> DocExpr
buildExpr (VarE s)       = Atom $ text $ varname s
buildExpr (FloatE d)     = Atom $ dtrunc d
buildExpr (StringE s)    = Atom $ doubleQuotes $ text s
buildExpr (IntE i)       = Atom $ int i
buildExpr (PfieldE i)    = Atom $ char 'p' <> int i
buildExpr (BinE op a b)  = Binary (buildExpr a) op (buildExpr b)
buildExpr (UnaryE op a)  = Unary op (buildExpr a)
buildExpr (FunCallE s a) = Atom $ text s <> parens (format a)
buildExpr (CondE p t f)  = Atom $ parens body
  where
    body = hsep [ format p, char '?', format t, char ':', format f ]

--------------------------------------------------------------------------------
-- L-values

data LVal rate = LVal Var
  deriving (Eq, Show)


rval :: LVal rate -> Expr rate
rval (LVal v) = Expr $ VarE v