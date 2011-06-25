{-# LANGUAGE ScopedTypeVariables        #-}
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

   -- Output
    out1
  , out2

  -- * Variable initialization
  , init
  , divz

  -- * Arithmetic and logical operators
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
import ZSnd.Core.Inst.Click
import ZSnd.Core.Utils.FormatExpr


import Prelude hiding ( init, negate, (&&), (||), (^)
                      , abs, exp, log, sqrt
                      , sin, cos, tan, sinh, cosh, tanh
                      , sum, product 
                      )




binop :: Rator -> Conf rate -> Conf rate -> Conf rate
binop op a b = mkConf $ CBinOp op (getConfI $ cast a) (getConfI $ cast b)

-- unop :: Rator -> Conf rate -> Conf rate
-- unop op a = mkConf $ CUnOp op (getConfI $ cast a)


out1 :: Conf ARate -> Element rate
out1 ain = mkElement "out" [ getConfA ain ] Out0

out2 :: Conf ARate -> Conf ARate -> Element rate
out2 ain bin = mkElement "outs" [ getConfA ain, getConfA bin ] Out0


init :: forall rate. (Rate rate) 
     => Conf IRate -> Element rate
init ia = 
    mkElement "init" [ getConfI ia ] 
                     (Out1 $ dataRate (undefined :: rate))
    




-- tival :: Expr IR
-- tival = Expr $ ZeroOp "tival"

divz :: forall rate. (Rate rate) 
     => Conf IRate -> Conf IRate -> Conf IRate -> Element rate
divz ia ib isubst = 
    mkElement "divz" [ getConfI ia, getConfI ib, getConfI isubst ]
                     (Out1 $ dataRate (undefined :: rate))
    


--------------------------------------------------------------------------------
-- Arithmetic and logical operators



infixr 8 ^
infixr 3 &&
infixr 2 ||


(&&) :: IK_Rate rate
     => Conf rate -> Conf rate -> Conf rate
(&&) = binop (infixL 3 "&&")

(||) :: IK_Rate rate 
     => Conf rate -> Conf rate -> Conf rate
(||) = binop (infixL 2 "||")

(^)  :: IK_Rate rate
     => Conf rate -> Conf rate -> Conf rate
(^)  = binop (infixL 4 "^")


infixl 7 %

(%) :: Conf rate -> Conf rate -> Conf rate
(%) = binop (infixL 7 "%")


--------------------------------------------------------------------------------
-- Math functions


-- | I or K rate only.
--
int       :: IK_Rate rate
          => Conf rate -> Conf rate
int       = funcall "int"
  

-- | I or K rate only.
--
frac      :: IK_Rate rate 
          => Conf rate -> Conf rate
frac      = funcall "frac"


-- | I or K rate only.
--
powoftwo  :: IK_Rate rate 
          => Conf rate -> Conf rate   
powoftwo  = funcall "powoftwo"


-- | I or K rate only.
--
logbtwo   :: IK_Rate rate => Conf rate -> Conf rate
logbtwo   = funcall "logbtwo"


-- | This is @i@ in Csound.
--
icast     :: Conf KRate -> Conf IRate
icast     = cast


abs       :: Conf rate -> Conf rate
abs       = funcall "abs"

exp       :: Conf rate -> Conf rate
exp       = funcall "exp"

log       :: Conf rate -> Conf rate
log       = funcall "log"

log10     :: Conf rate -> Conf rate
log10     = funcall "log10"

sqrt      :: Conf rate -> Conf rate
sqrt      = funcall "sqrt"


--------------------------------------------------------------------------------
-- Trig functions

sin       :: Conf rate -> Conf rate
sin       = funcall "sin"

cos       :: Conf rate -> Conf rate
cos       = funcall "cos"

tan       :: Conf rate -> Conf rate
tan       = funcall "tan"


sininv    :: Conf rate -> Conf rate
sininv    = funcall "sininv"

cosinv    :: Conf rate -> Conf rate
cosinv    = funcall "cosinv"

taninv    :: Conf rate -> Conf rate
taninv    = funcall "taninv"

sinh      :: Conf rate -> Conf rate
sinh      = funcall "sinh"

cosh      :: Conf rate -> Conf rate
cosh      = funcall "cosh"

tanh      :: Conf rate -> Conf rate
tanh      = funcall "tanh"

--------------------------------------------------------------------------------
-- Amplitude functions

-- | I or K rate only.
--
dbamp     :: IK_Rate rate 
          => Conf rate -> Conf rate
dbamp     = funcall "dbamp"


-- | 
ampdb     :: Conf rate -> Conf rate
ampdb     = funcall "ampdb"

--------------------------------------------------------------------------------
-- Random functions

-- | I or K rate only.
--
rnd       :: IK_Rate rate 
          => Conf rate -> Conf rate
rnd       = funcall "rnd"
 
-- | I or K rate only.
--
birnd     :: IK_Rate rate 
          => Conf rate -> Conf rate
birnd     = funcall "birnd"


--------------------------------------------------------------------------------
-- Opcode equivalents of functions

sum :: [Conf ARate] -> Element ARate
sum xs = 
    mkElement "sum" (map getConfA xs) (Out1 A)


product :: [Conf ARate] -> Element ARate
product xs = 
    mkElement "product" (map getConfA xs) (Out1 A)

pow :: forall rate. (Rate rate) 
    => Conf rate -> Conf rate -> Element rate
pow a b = 
    mkElement "pow" [ getConfUniv a, getConfUniv b ]
                    (Out1 $ dataRate (undefined::rate))


taninv2 :: forall rate. (Rate rate) 
        => Conf rate -> Conf rate -> Element rate
taninv2 a b = 
    mkElement "taninv2" [ getConfUniv a, getConfUniv b ]
                        (Out1 $ dataRate (undefined::rate))


mac :: [(Conf ARate, Conf KRate)] -> Element ARate
mac xs = 
    mkElement "mac" (concatMap fn xs) (Out1 A)
  where
    fn (a,b) = [ getConfA a, getConfK b ]

maca :: [Conf ARate] -> Element ARate
maca xs = 
    mkElement "maca" (map getConfA xs) (Out1 A)

--------------------------------------------------------------------------------
-- Pitch conversion


-- | Convert a pitch-class value to octave-point-decimal.
-- 
-- I or K rate only.
--
octpch :: IK_Rate rate 
       => Conf rate -> Conf rate
octpch = funcall "octpch"


-- | I or K rate only.
--
cpspch :: IK_Rate rate 
       => Conf rate -> Conf rate
cpspch = funcall "cpspch"

-- | I or K rate only.
--
pchoct :: IK_Rate rate 
       => Conf rate -> Conf rate
pchoct = funcall "pchoct"


-- | I or K rate only.
octcps :: IK_Rate rate 
       => Conf rate -> Conf rate
octcps = funcall "octcps"



-- | No rate restriction.
--
cpsoct    :: Conf rate -> Conf rate
cpsoct    = funcall "cpsoct"


-- Design note - here Csound uses @x@ for the version with more 
-- args. Is this a convention used elsewhere?

cps2pch :: Conf IRate -> Conf IRate -> Element IRate
cps2pch ipch ieq = 
    mkElement "cps2pch" [ getConfI ipch, getConfI ieq ] (Out1 I)

cpsxpch :: Conf IRate -> Conf IRate -> Conf IRate -> Conf IRate 
        -> Element IRate
cpsxpch ipch ieq irep ibase = 
    mkElement "cps2pch" [ getConfI ipch, getConfI ieq
                        , getConfI irep, getConfI ibase ]
                        (Out1 I)
