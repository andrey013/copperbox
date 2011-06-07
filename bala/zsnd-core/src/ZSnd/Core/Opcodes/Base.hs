{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSnd.Core.Opcodes.Base
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Basic opcodes and function.
--
--------------------------------------------------------------------------------

module ZSnd.Core.Opcodes.Base
  (

  -- * Variable initialization
    init
  , tival
  , divz

  -- * Arithmetic and logical operators
  , negate
  , (&&)
  , (||) 
  , (^)
  , (%)

  -- * Math functions
  , int
  , frac
  , icast
  , abs
  , exp
  , log
  , log10
  , sqrt
  , powoftwo
  , logbtwo

  -- * Trig functions
  , sin
  , cos
  , tan
  , sininv
  , cosinv
  , taninv
  , sinh
  , cosh
  , tanh

  -- * Amplitude functions
  , dbamp
  , ampdb

  -- * Random functions
  , rnd
  , birnd

  -- * Opcode equivalents of functions
  , sum
  , product
  , pow
  , taninv2
  , mac
  , maca

  -- * Pitch converots
  , octpch
  , cpspch
  , pchoct
  , octcps
  , cpsoct

  , cps2pch
  , cpsxpch


  ) where


import ZSnd.Core.CsoundInst

import Prelude hiding ( init, negate, (&&), (||), (^)
                      , abs, exp, log, sqrt
                      , sin, cos, tan, sinh, cosh, tanh
                      , sum, product 
                      )


init :: Expr IR -> Expr a
init (Expr a) = Expr a

tival :: Expr IR
tival = Expr $ ZeroOp "tival"

divz :: Opcode rate
     => Expr IR -> Expr IR -> Expr IR -> InstBuilder (Expr rate)
divz ia ib isubst = 
    opcode "divz" [ getExpr ia, getExpr ib, getExpr isubst ]

--------------------------------------------------------------------------------
-- Arithmetic and logical operators

negate :: Expr a -> Expr a
negate = Expr . Funcall "-" . getExpr

infixr 8 ^
infixr 3 &&
infixr 2 ||


(&&) :: IK_Rate rate
     => Expr rate -> Expr rate -> Expr rate
(&&) a b = Expr $ BinOp "&&" (getExpr a) (getExpr b)

(||) :: IK_Rate rate 
     => Expr rate -> Expr rate -> Expr rate
(||) a b = Expr $ BinOp "||" (getExpr a) (getExpr b)

(^)  :: IK_Rate rate
     => Expr rate -> Expr rate -> Expr rate
(^)  a b = Expr $ BinOp "^"  (getExpr a) (getExpr b)


infixl 7 %

(%) :: Expr a -> Expr a -> Expr a
(%) a b = Expr $ BinOp "%" (getExpr a) (getExpr b)

--------------------------------------------------------------------------------
-- Math functions


-- | I or K rate only.
--
int       :: IK_Rate rate => Expr rate -> Expr rate
int       = Expr . Funcall "int" . getExpr
  

-- | I or K rate only.
--
frac      :: IK_Rate rate => Expr rate -> Expr rate
frac      = Expr . Funcall "frac" . getExpr


-- | I or K rate only.
--
powoftwo  :: IK_Rate rate => Expr rate -> Expr rate   
powoftwo  = Expr . Funcall "powoftwo" . getExpr

-- | I or K rate only.
--
logbtwo   :: IK_Rate rate => Expr rate -> Expr rate
logbtwo   = Expr . Funcall "logbtwo" . getExpr


-- | This is @i@ in Csound.
--
icast :: Expr KR -> Expr IR
icast (Expr a) = Expr a


abs :: Expr rate -> Expr rate
abs = Expr . Funcall "abs" . getExpr

exp :: Expr rate -> Expr rate
exp = Expr . Funcall "exp" . getExpr

log :: Expr rate -> Expr rate
log = Expr . Funcall "log" . getExpr

log10 :: Expr rate -> Expr rate
log10 = Expr . Funcall "log10" . getExpr

sqrt :: Expr rate -> Expr rate
sqrt = Expr . Funcall "sqrt" . getExpr


--------------------------------------------------------------------------------
-- Trig functions

sin :: Expr rate -> Expr rate
sin = Expr . Funcall "sin" . getExpr

cos :: Expr rate -> Expr rate
cos = Expr . Funcall "cos" . getExpr

tan :: Expr rate -> Expr rate
tan = Expr . Funcall "tan" . getExpr


sininv :: Expr rate -> Expr rate
sininv = Expr . Funcall "sininv" . getExpr

cosinv :: Expr rate -> Expr rate
cosinv = Expr . Funcall "cosinv" . getExpr

taninv :: Expr rate -> Expr rate
taninv = Expr . Funcall "taninv" . getExpr

sinh :: Expr rate -> Expr rate
sinh = Expr . Funcall "sinh" . getExpr

cosh :: Expr rate -> Expr rate
cosh = Expr . Funcall "cosh" . getExpr

tanh :: Expr rate -> Expr rate
tanh = Expr . Funcall "tanh" . getExpr

--------------------------------------------------------------------------------
-- Amplitude functions

-- | I or K rate only.
--
dbamp     :: IK_Rate rate => Expr rate -> Expr rate
dbamp     = Expr . Funcall "dbamp" . getExpr


-- | 
ampdb     :: Expr a -> Expr a
ampdb     = Expr . Funcall "ampdb" . getExpr

--------------------------------------------------------------------------------
-- Random functions

-- | I or K rate only.
--
rnd       :: IK_Rate rate => Expr rate -> Expr rate
rnd       = Expr . Funcall "rnd" . getExpr
 
-- | I or K rate only.
--
birnd     :: IK_Rate rate => Expr rate -> Expr rate
birnd     = Expr . Funcall "birnd" . getExpr


--------------------------------------------------------------------------------
-- Opcode equivalents of functions

sum :: [Expr AR] -> InstBuilder (Expr AR)
sum = opcode "sum" . map getExpr

product :: [Expr AR] -> InstBuilder (Expr AR)
product = opcode "product" . map getExpr

pow :: Opcode rate
    => Expr rate -> Expr rate -> InstBuilder (Expr rate)
pow a b = opcode "pow" [ getExpr a, getExpr b ]


taninv2 :: Opcode rate
        => Expr rate -> Expr rate -> InstBuilder (Expr rate)
taninv2 a b = opcode "taninv2" [ getExpr a, getExpr b ]

mac :: [(Expr AR, Expr KR)] -> InstBuilder (Expr AR)
mac = opcode "mac" . concatMap fn
  where
    fn (a,b) = [ getExpr a, getExpr b ]

maca :: [Expr AR] -> InstBuilder (Expr AR)
maca = opcode "maca" . map getExpr

--------------------------------------------------------------------------------
-- Pitch conversion


-- | Convert a pitch-class value to octave-point-decimal.
-- 
-- I or K rate only.
--
octpch :: IK_Rate rate => Expr rate -> Expr rate
octpch = Expr . Funcall "octpch" . getExpr


-- | I or K rate only.
--
cpspch :: IK_Rate rate => Expr rate -> Expr rate
cpspch = Expr . Funcall "cpspch" . getExpr

-- | I or K rate only.
--
pchoct :: IK_Rate rate => Expr rate -> Expr rate
pchoct = Expr . Funcall "pchoct" . getExpr


-- | I or K rate only.
octcps :: IK_Rate rate => Expr rate -> Expr rate
octcps = Expr . Funcall "octcps" . getExpr



-- | No rate restriction.
--
cpsoct    :: Expr a -> Expr a
cpsoct    = Expr . Funcall "cpsoct" . getExpr

-- Design note - here Csound uses @x@ for the version with more 
-- args. Is this a convention used elsewhere?

cps2pch :: Expr IR -> Expr IR -> InstBuilder (Expr IR)
cps2pch ipch ieq = 
    opcode "cps2pch" [ getExpr ipch, getExpr ieq ]


cpsxpch :: Expr IR -> Expr IR -> Expr IR -> Expr IR 
        -> InstBuilder (Expr IR)
cpsxpch ipch ieq irep ibase = 
    opcode "cps2pch" [ getExpr ipch, getExpr ieq, getExpr irep, getExpr ibase ]

