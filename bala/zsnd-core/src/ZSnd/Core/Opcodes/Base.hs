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
-- Csound opcodes.
-- 
-- Unlike Haskell, Csound allows optional arguments in opcode 
-- signatures. ZSnd models this by defining two versions of 
-- such opcodes:
-- 
-- a) The short version with no optional args uses the regular 
-- Csound name.
-- 
-- b) The fully specified version, which includes all optional
-- args, suffixes the Csound name with an underscore.
--
-- Note, there are many name clashes with Prelude. 
--
-- Also note, some opcodes are actually class methods to support 
-- rate overloading. However, whilst the class methods are 
-- exported, the class isn'\t. This is because the set of 
-- instances is finite - one or more of @IRate@, @KRate@ or
-- @ARate@.
-- 
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

divz :: Expr IR -> Expr IR -> Expr IR -> InstBuilder (Expr a)
divz ia ib isubst = 
    opcode "divz" [ getExpr ia, getExpr ib, getExpr isubst ]

--------------------------------------------------------------------------------
-- Arithmetic and logical operators

negate :: Expr a -> Expr a
negate = Expr . Funcall "-" . getExpr

infixr 8 ^
infixr 3 &&
infixr 2 ||

class CArith rate where
  (&&) :: Expr rate -> Expr rate -> Expr rate
  (||) :: Expr rate -> Expr rate -> Expr rate
  (^)  :: Expr rate -> Expr rate -> Expr rate

instance CArith IR where
  (&&) a b = Expr $ BinOp "&&" (getExpr a) (getExpr b)
  (||) a b = Expr $ BinOp "||" (getExpr a) (getExpr b)
  (^)  a b = Expr $ BinOp "^"  (getExpr a) (getExpr b)

instance CArith KR where
  (&&) a b = Expr $ BinOp "&&" (getExpr a) (getExpr b)
  (||) a b = Expr $ BinOp "||" (getExpr a) (getExpr b)
  (^)  a b = Expr $ BinOp "^"  (getExpr a) (getExpr b)



infixl 7 %

(%) :: Expr a -> Expr a -> Expr a
(%) a b = Expr $ BinOp "%" (getExpr a) (getExpr b)

--------------------------------------------------------------------------------
-- Math functions

class CMath rate where
  -- | I or K rate only.
  int       :: Expr rate -> Expr rate

  -- | I or K rate only.
  frac      :: Expr rate -> Expr rate

  -- | I or K rate only.
  powoftwo  :: Expr rate -> Expr rate   

  -- | I or K rate only.
  logbtwo   :: Expr rate -> Expr rate

instance CMath IR where
  int       = Expr . Funcall "int" . getExpr
  frac      = Expr . Funcall "frac" . getExpr
  powoftwo  = Expr . Funcall "powoftwo" . getExpr
  logbtwo   = Expr . Funcall "logbtwo" . getExpr

instance CMath KR where
  int       = Expr . Funcall "int" . getExpr
  frac      = Expr . Funcall "frac" . getExpr
  powoftwo  = Expr . Funcall "powoftwo" . getExpr
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

class CAmp rate where
  -- | I or K rate only.
  dbamp     :: Expr rate -> Expr rate

instance CAmp IR where
  dbamp     = Expr . Funcall "dbamp" . getExpr

instance CAmp KR where
  dbamp     = Expr . Funcall "dbamp" . getExpr

-- | 
ampdb     :: Expr a -> Expr a
ampdb     = Expr . Funcall "ampdb" . getExpr

--------------------------------------------------------------------------------
-- Random functions

class CRnd rate where
  -- | I or K rate only.
  rnd       :: Expr rate -> Expr rate
 
  -- | I or K rate only.
  birnd     :: Expr rate -> Expr rate

instance CRnd IR where
  rnd       = Expr . Funcall "rnd" . getExpr
  birnd     = Expr . Funcall "birnd" . getExpr

instance CRnd KR where
  rnd       = Expr . Funcall "rnd" . getExpr
  birnd     = Expr . Funcall "birnd" . getExpr

--------------------------------------------------------------------------------
-- Opcode equivalents of functions

sum :: [Expr AR] -> InstBuilder (Expr AR)
sum = aopcode "sum" . map getExpr

product :: [Expr AR] -> InstBuilder (Expr AR)
product = aopcode "product" . map getExpr

pow :: Expr a -> Expr a -> InstBuilder (Expr a)
pow a b = opcode "pow" [ getExpr a, getExpr b ]


taninv2 :: Expr a -> Expr a -> InstBuilder (Expr a)
taninv2 a b = opcode "taninv2" [ getExpr a, getExpr b ]

mac :: [(Expr AR, Expr KR)] -> InstBuilder (Expr AR)
mac = aopcode "mac" . concatMap fn
  where
    fn (a,b) = [ getExpr a, getExpr b ]

maca :: [Expr AR] -> InstBuilder (Expr AR)
maca = aopcode "maca" . map getExpr

--------------------------------------------------------------------------------
-- Pitch conversion

class CPitchConvert rate where

  -- | Convert a pitch-class value to octave-point-decimal.
  -- 
  -- I or K rate only.
  --
  octpch :: Expr rate -> Expr rate

  -- | I or K rate only.
  cpspch :: Expr rate -> Expr rate

  -- | I or K rate only.
  pchoct :: Expr rate -> Expr rate

  -- | I or K rate only.
  octcps :: Expr rate -> Expr rate



instance CPitchConvert IR where
  octpch    = Expr . Funcall "octpch" . getExpr
  cpspch    = Expr . Funcall "cpspch" . getExpr
  pchoct    = Expr . Funcall "pchoct" . getExpr
  octcps    = Expr . Funcall "octcps" . getExpr

instance CPitchConvert KR where
  octpch    = Expr . Funcall "octpch" . getExpr
  cpspch    = Expr . Funcall "cpspch" . getExpr
  pchoct    = Expr . Funcall "pchoct" . getExpr
  octcps    = Expr . Funcall "octcps" . getExpr


-- | No rate restriction.
--
cpsoct    :: Expr a -> Expr a
cpsoct    = Expr . Funcall "cpsoct" . getExpr

-- Design note - here Csound uses @x@ for the version with more 
-- args. Is this a convention used elsewhere?

cps2pch :: Expr IR -> Expr IR -> InstBuilder (Expr IR)
cps2pch ipch ieq = 
    iopcode "cps2pch" [ getExpr ipch, getExpr ieq ]


cpsxpch :: Expr IR -> Expr IR -> Expr IR -> Expr IR 
        -> InstBuilder (Expr IR)
cpsxpch ipch ieq irep ibase = 
    iopcode "cps2pch" [ getExpr ipch, getExpr ieq, getExpr irep, getExpr ibase ]

