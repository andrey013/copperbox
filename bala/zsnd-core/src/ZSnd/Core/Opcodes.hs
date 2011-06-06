{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSnd.Core.Opcodes
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

module ZSnd.Core.Opcodes
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

  -- * Linear and exponential generators
  , line
  , expon 
  , linseg
  , linsegr
  , expseg
  , expsegr
  , expsega
  , adsr
  , adsr_


  , oscil

  ) where


import ZSnd.Core.CsoundInst

import Prelude hiding ( init, negate, (&&), (||), (^)
                      , abs, exp, log, sqrt
                      , sin, cos, tan, sinh, cosh, tanh
                      , sum, product 
                      )


init :: Expr IRate -> Expr a
init (Expr a) = Expr a

tival :: Expr IRate
tival = Expr $ ZeroOp "tival"

divz :: Expr IRate -> Expr IRate -> Expr IRate -> InstBuilder (Expr a)
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

instance CArith IRate where
  (&&) a b = Expr $ BinOp "&&" (getExpr a) (getExpr b)
  (||) a b = Expr $ BinOp "||" (getExpr a) (getExpr b)
  (^)  a b = Expr $ BinOp "^"  (getExpr a) (getExpr b)

instance CArith KRate where
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

instance CMath IRate where
  int       = Expr . Funcall "int" . getExpr
  frac      = Expr . Funcall "frac" . getExpr
  powoftwo  = Expr . Funcall "powoftwo" . getExpr
  logbtwo   = Expr . Funcall "logbtwo" . getExpr

instance CMath KRate where
  int       = Expr . Funcall "int" . getExpr
  frac      = Expr . Funcall "frac" . getExpr
  powoftwo  = Expr . Funcall "powoftwo" . getExpr
  logbtwo   = Expr . Funcall "logbtwo" . getExpr

-- | This is @i@ in Csound.
--
icast :: Expr KRate -> Expr IRate
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

instance CAmp IRate where
  dbamp     = Expr . Funcall "dbamp" . getExpr

instance CAmp KRate where
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

instance CRnd IRate where
  rnd       = Expr . Funcall "rnd" . getExpr
  birnd     = Expr . Funcall "birnd" . getExpr

instance CRnd KRate where
  rnd       = Expr . Funcall "rnd" . getExpr
  birnd     = Expr . Funcall "birnd" . getExpr

--------------------------------------------------------------------------------
-- Opcode equivalents of functions

sum :: [Expr ARate] -> InstBuilder (Expr ARate)
sum = aopcode "sum" . map getExpr

product :: [Expr ARate] -> InstBuilder (Expr ARate)
product = aopcode "product" . map getExpr

pow :: Expr a -> Expr a -> InstBuilder (Expr a)
pow a b = opcode "pow" [ getExpr a, getExpr b ]


taninv2 :: Expr a -> Expr a -> InstBuilder (Expr a)
taninv2 a b = opcode "taninv2" [ getExpr a, getExpr b ]

mac :: [(Expr ARate, Expr KRate)] -> InstBuilder (Expr ARate)
mac = aopcode "mac" . concatMap fn
  where
    fn (a,b) = [ getExpr a, getExpr b ]

maca :: [Expr ARate] -> InstBuilder (Expr ARate)
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



instance CPitchConvert IRate where
  octpch    = Expr . Funcall "octpch" . getExpr
  cpspch    = Expr . Funcall "cpspch" . getExpr
  pchoct    = Expr . Funcall "pchoct" . getExpr
  octcps    = Expr . Funcall "octcps" . getExpr

instance CPitchConvert KRate where
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

cps2pch :: Expr IRate -> Expr IRate -> InstBuilder (Expr IRate)
cps2pch ipch ieq = 
    iopcode "cps2pch" [ getExpr ipch, getExpr ieq ]


cpsxpch :: Expr IRate -> Expr IRate -> Expr IRate -> Expr IRate 
        -> InstBuilder (Expr IRate)
cpsxpch ipch ieq irep ibase = 
    iopcode "cps2pch" [ getExpr ipch, getExpr ieq, getExpr irep, getExpr ibase ]


--------------------------------------------------------------------------------

-- MIDI is ignored at least for the time being

--------------------------------------------------------------------------------
-- Linear and Exponential generators

class CLinear rate where
  line      :: Expr IRate -> Expr IRate -> Expr IRate -> InstBuilder (Expr rate)
  expon     :: Expr IRate -> Expr IRate -> Expr IRate -> InstBuilder (Expr rate)
  linseg    :: Expr IRate -> Expr IRate -> Expr IRate 
                          -> [(Expr IRate, Expr IRate)]
                          -> InstBuilder (Expr rate)

  linsegr   :: Expr IRate -> Expr IRate -> Expr IRate 
                          -> [(Expr IRate, Expr IRate)]
                          -> Expr IRate -> Expr IRate 
                          -> InstBuilder (Expr rate)

  expseg    :: Expr IRate -> Expr IRate -> Expr IRate 
                          -> [(Expr IRate, Expr IRate)]
                          -> InstBuilder (Expr rate)

  expsegr   :: Expr IRate -> Expr IRate -> Expr IRate 
                          -> [(Expr IRate, Expr IRate)]
                          -> Expr IRate -> Expr IRate 
                          -> InstBuilder (Expr rate)



instance CLinear KRate where
  line ia idur ib = kopcode "line" [ getExpr ia, getExpr idur, getExpr ib ]

  expon ia idur ib = kopcode "expon" [ getExpr ia, getExpr idur, getExpr ib ]

  linseg ia idur ib xs = 
    kopcode "linseg" (getExpr ia : getExpr idur : getExpr ib : rest)
      where
        rest = concatMap (\(a,b) -> [getExpr a, getExpr b]) xs

  linsegr ia idur ib xs irel iz = 
    kopcode "linsegr" (getExpr ia : getExpr idur : getExpr ib : rest ++ end)
      where
        rest = concatMap (\(a,b) -> [getExpr a, getExpr b]) xs
        end  = [getExpr irel, getExpr iz]

  expseg ia idur ib xs = 
    kopcode "expseg" (getExpr ia : getExpr idur : getExpr ib : rest)
      where
        rest = concatMap (\(a,b) -> [getExpr a, getExpr b]) xs

  expsegr ia idur ib xs irel iz = 
    kopcode "linsegr" (getExpr ia : getExpr idur : getExpr ib : rest ++ end)
      where
        rest = concatMap (\(a,b) -> [getExpr a, getExpr b]) xs
        end  = [getExpr irel, getExpr iz]


instance CLinear ARate where
  line ia idur ib = aopcode "line" [ getExpr ia, getExpr idur, getExpr ib ]

  expon ia idur ib = aopcode "expon" [ getExpr ia, getExpr idur, getExpr ib ]

  linseg ia idur ib xs = 
    aopcode "linseg" (getExpr ia : getExpr idur : getExpr ib : rest)
      where
        rest = concatMap (\(a,b) -> [getExpr a, getExpr b]) xs

  linsegr ia idur ib xs irel iz = 
    aopcode "linsegr" (getExpr ia : getExpr idur : getExpr ib : rest ++ end)
      where
        rest = concatMap (\(a,b) -> [getExpr a, getExpr b]) xs
        end  = [getExpr irel, getExpr iz]

  expseg ia idur ib xs = 
    aopcode "expseg" (getExpr ia : getExpr idur : getExpr ib : rest)
      where
        rest = concatMap (\(a,b) -> [getExpr a, getExpr b]) xs

  expsegr ia idur ib xs irel iz = 
    aopcode "expsegr" (getExpr ia : getExpr idur : getExpr ib : rest ++ end)
      where
        rest = concatMap (\(a,b) -> [getExpr a, getExpr b]) xs
        end  = [getExpr irel, getExpr iz]



expsega   :: Expr IRate -> Expr IRate -> Expr IRate 
                        -> [(Expr IRate, Expr IRate)]
                        -> InstBuilder (Expr ARate)
expsega ia idur ib xs = 
    aopcode "expsega" (getExpr ia : getExpr idur : getExpr ib : rest)
  where
    rest = concatMap (\(a,b) -> [getExpr a, getExpr b]) xs


class CAdsr rate where
  adsr      :: Expr IRate -> Expr IRate -> Expr IRate -> Expr IRate
                          -> InstBuilder (Expr rate)
  adsr_     :: Expr IRate -> Expr IRate -> Expr IRate -> Expr IRate
                          -> Expr IRate -> InstBuilder (Expr rate)

instance CAdsr KRate where
  adsr ia idec isl ir = 
    kopcode "adsr" [ getExpr ia, getExpr idec, getExpr isl, getExpr ir ]

  adsr_ ia idec isl ir idel = 
    kopcode "adsr" [ getExpr ia, getExpr idec, getExpr isl
                   , getExpr ir, getExpr idel ]

instance CAdsr ARate where
  adsr ia idec isl ir = 
    aopcode "adsr" [ getExpr ia, getExpr idec, getExpr isl, getExpr ir ]

  adsr_ ia idec isl ir idel = 
    aopcode "adsr" [ getExpr ia, getExpr idec, getExpr isl
                   , getExpr ir, getExpr idel ]


--------------------------------------------------------------------------------

oscil :: Expr a -> Expr a -> Expr a -> InstBuilder (Expr ARate)
oscil amp cps ifn = aopcode "oscil" [getExpr amp, getExpr cps, getExpr ifn]
