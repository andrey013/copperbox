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

  , DataRate

  , Rate        -- opaque
  , dataRate
  , KA_Rate
  , IK_Rate

  -- * Expressions
  , Expr
  , liftE1
  , liftE2

  , DExpr(..)

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
  deriving (Bounded,Enum,Eq,Ord,Show)



-- Note - the classes are exported opaquely. This is because there
-- is a fixed, closed set of instances.


class Rate rate where
  dataRate :: rate -> DataRate

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

newtype Expr rate = Expr { getExpr :: DExpr }
  deriving (Eq, Show)


-- | String data is used by @soundin@ for example.
--
data DExpr = VarE String
           | FloatE  Double
           | StringE String    
           | IntE    Int
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
buildExpr (VarE s)       = Atom $ text s
buildExpr (FloatE d)     = Atom $ dtrunc d
buildExpr (StringE s)    = Atom $ doubleQuotes $ text s
buildExpr (IntE i)       = Atom $ int i
buildExpr (BinE op a b)  = Binary (buildExpr a) op (buildExpr b)
buildExpr (UnaryE op a)  = Unary op (buildExpr a)
buildExpr (FunCallE s a) = Atom $ text s <> parens (format a)
buildExpr (CondE p t f)  = Atom $ parens body
  where
    body = hsep [ format p, char '?', format t, char ':', format f ]

