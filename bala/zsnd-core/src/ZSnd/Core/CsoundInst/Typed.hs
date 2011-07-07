{-# LANGUAGE EmptyDataDecls             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSnd.Core.CsoundInst.Typed
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- New inst langauge...
--
--------------------------------------------------------------------------------

module ZSnd.Core.CsoundInst.Typed
  (
    IInit
  , KRate
  , ARate
  , ITableNum

  , IR
  , KR
  , AR

  , Rate        -- opaque
  , dataRate
  , KA_Rate
  , IK_Rate
  , Expr_Rate

  -- * Phantom typed Expr
  , Expr
  , getExprI
  , getExprK
  , getExprA
  , getExprUniv

  , binop
  , unop
  
  , cast
  , pfield
  , funcall  
  , tablefn
  , filecode

  -- * Phantom typed Var
  , Var
  , mkVar
  , getVarI
  , getVarK
  , getVarA
  , getVarUniv

  , var

  , Opcode0(..)
  , assignStmt0

  , Opcode1(..)
  , assignStmt1

  , Opcode2(..)
  , assignStmt2

  , Opcode3(..)
  , assignStmt3

  , Opcode4(..)
  , assignStmt4

  ) where

import ZSnd.Core.CsoundInst.Prim
import ZSnd.Core.Utils.FormatExpr

data IInit
data KRate
data ARate

data ITableNum          


type IR = Expr IInit
type KR = Expr KRate
type AR = Expr ARate


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


instance Rate ITableNum where
  dataRate _ = I


class Rate rate => KA_Rate rate

instance KA_Rate KRate
instance KA_Rate ARate


class Rate rate => IK_Rate rate

instance IK_Rate IInit
instance IK_Rate KRate


-- | No ITableNum instance - this means table ref fields cannot 
-- be added etc.
--
class Rate rate => Expr_Rate rate

instance Expr_Rate IInit
instance Expr_Rate KRate
instance Expr_Rate ARate



--------------------------------------------------------------------------------
-- Conf

-- | Typed version of 'UExpr' - rate is a phantom param.
--
newtype Expr rate = Expr { getExpr :: UExpr }
  deriving (Eq,Ord)

instance Show (Expr rate) where
  showsPrec p = showsPrec p . getExpr



getExprI :: Expr IInit -> UExpr
getExprI = getExpr

getExprK :: Expr KRate -> UExpr
getExprK = getExpr

getExprA :: Expr ARate -> UExpr
getExprA = getExpr

getExprUniv :: Expr rate -> UExpr
getExprUniv = getExpr


-- Constructors ( also num instances for literals)


cast :: Expr r1 -> Expr r2
cast a = Expr (getExpr a)


pfield  :: Int -> Expr rate
pfield  = Expr . PField


funcall :: String -> Expr rate -> Expr rate
funcall s a = Expr $ Funcall s (getExpr a)

tablefn  :: Int -> Expr ITableNum
tablefn = Expr . TableRef 

filecode  :: String -> Expr IInit
filecode = Expr . Literal . CsString



binop :: Rator -> Expr rate -> Expr rate -> Expr rate
binop op a b = Expr $ BinOp op (getExpr a) (getExpr b)

unop :: Rator -> Expr rate -> Expr rate
unop op a = Expr $ UnOp op (getExpr a)


-- Note - Num instances exclude ITableNum (table refs)

instance Expr_Rate rate => Num (Expr rate) where
  (+)     = binop plus_op
  (-)     = binop minus_op
  (*)     = binop mult_op
  abs     = funcall "abs"
  negate  = unop unary_negate
  signum _      = error "signum - no interpretation of signum in Csound."
  fromInteger i = Expr $ Literal $ CsInt $ fromInteger i


instance Expr_Rate rate => Fractional (Expr rate) where
  (/)     = binop divide_op
  recip _ = error "recip - no interpretation of recip in Csound."  
  fromRational d = Expr $ Literal $ CsDouble (fromRational d)



--------------------------------------------------------------------------------
-- Variable

newtype Var rate = Var { getVar :: VarId } 
  deriving (Eq,Ord)

instance Show (Var rate) where
  showsPrec p = showsPrec p . getVar

mkVar :: VarId -> Var rate
mkVar = Var

getVarI :: Var IInit -> VarId
getVarI = getVar

getVarK :: Var KRate -> VarId
getVarK = getVar

getVarA :: Var ARate -> VarId
getVarA = getVar

getVarUniv :: Var rate -> VarId
getVarUniv = getVar


var :: Var rate -> Expr rate
var v1 = Expr $ VarE (getVarUniv v1)


--------------------------------------------------------------------------------
-- Opcode

-- | Opcode generating 1 output channel.
--
data Opcode0 rate = Opcode0 String [UExpr]
  deriving (Eq,Ord,Show)

-- Is this one necessary - check @delayw@...
--
assignStmt0 :: Opcode0 rate -> UStmt
assignStmt0 (Opcode0 name exprs) = 
    OpcodeAssign [] name exprs



-- | Opcode generating 1 output channel.
--
data Opcode1 rate = Opcode1 String [UExpr]
  deriving (Eq,Ord,Show)


assignStmt1 :: Var rate -> Opcode1 rate -> UStmt
assignStmt1 v1 (Opcode1 name exprs) = 
    OpcodeAssign [getVarUniv v1] name exprs


-- | Opcode generating 2 output channels.
--
data Opcode2 rate = Opcode2 String [UExpr]
  deriving (Eq,Ord,Show)


assignStmt2 :: (Var rate,Var rate) -> Opcode2 rate -> UStmt
assignStmt2 (v1,v2) (Opcode2 name exprs) = 
    OpcodeAssign [getVarUniv v1, getVarUniv v2] name exprs



-- | Opcode generating 3 results - e.g. 'lorenz'.
--
data Opcode3 rate = Opcode3 String [UExpr]
  deriving (Eq,Ord,Show)


assignStmt3 :: (Var rate,Var rate,Var rate) -> Opcode3 rate -> UStmt
assignStmt3 (v1,v2,v3) (Opcode3 name exprs) = 
    OpcodeAssign [getVarUniv v1, getVarUniv v2, getVarUniv v3] name exprs



-- | Opcode generating 4 output channels.
--
data Opcode4 rate = Opcode4 String [UExpr]
  deriving (Eq,Ord,Show)


assignStmt4 :: (Var rate,Var rate, Var rate, Var rate) -> Opcode4 rate 
            -> UStmt
assignStmt4 (v1,v2,v3,v4) (Opcode4 name exprs) = 
    OpcodeAssign xs name exprs 
  where
    xs = [ getVarUniv v1, getVarUniv v2, getVarUniv v3, getVarUniv v4 ]

