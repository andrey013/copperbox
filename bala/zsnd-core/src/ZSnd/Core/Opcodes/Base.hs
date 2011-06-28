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


import ZSnd.Core.CsoundInst.Click
import ZSnd.Core.CsoundInst.Index
import ZSnd.Core.CsoundInst.Prim
import ZSnd.Core.CsoundInst.Typed


import Prelude hiding ( init, negate, (&&), (||), (^)
                      , abs, exp, log, sqrt
                      , sin, cos, tan, sinh, cosh, tanh
                      , sum, product 
                      )




binop :: Rator -> Conf rate -> Conf rate -> Conf rate
binop op a b = mkConf $ BinOp op (getConfI $ cast a) (getConfI $ cast b)

-- unop :: Rator -> Conf rate -> Conf rate
-- unop op a = mkConf $ CUnOp op (getConfI $ cast a)


out1 :: Opcode1 ARate -> Element rate
out1 opF = 
    mkElement "out" inspec Out0 
  where
    inspec = applyOpcode opF $ \ ain -> 
               [ getConfA ain ]

out2 :: Opcode2 ARate ARate -> Element rate
out2 opF = 
    mkElement "outs" inspec Out0
  where
    inspec = applyOpcode opF $ \(ain1, ain2) -> 
               [ getConfA ain1, getConfA ain2 ]


init :: forall rate. (Rate rate) 
     => Opcode1 IRate -> Element rate
init opF = 
    mkElement "init" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \ia -> 
               [ getConfI ia ] 




-- tival :: Expr IR
-- tival = Expr $ ZeroOp "tival"

divz :: forall rate. (Rate rate) 
     => Opcode3 IRate IRate IRate -> Element rate
divz opF =
    mkElement "divz" inspec (Out1 $ dataRate (undefined :: rate))
  where
    inspec = applyOpcode opF $ \ (ia, ib, isubst) -> 
               [ getConfI ia, getConfI ib, getConfI isubst ]
                     
    


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


-- Not sure how to handle these...

sum :: OpcodeList1 ARate -> Element ARate
sum opF = 
    mkElement "sum" inspec (Out1 A)
  where
    inspec = mapOpcode opF $ \x -> 
               (getConfA x) 


product :: OpcodeList1 ARate -> Element ARate
product opF = 
    mkElement "product" inspec (Out1 A)
  where
    inspec = mapOpcode opF $ \x -> 
               (getConfA x) 



pow :: forall rate. (Rate rate) 
    => Opcode2 rate rate -> Element rate
pow opF = 
    mkElement "pow" inspec (Out1 $ dataRate (undefined::rate))
  where
    inspec = applyOpcode opF $ \(a,b) -> 
               [ getConfUniv a, getConfUniv b ]

taninv2 :: forall rate. (Rate rate) 
        => Opcode2 rate rate -> Element rate
taninv2 opF = 
    mkElement "taninv2" inspec (Out1 $ dataRate (undefined::rate))
  where
    inspec = applyOpcode opF $ \(a,b) -> 
               [ getConfUniv a, getConfUniv b ]


mac :: OpcodeList2 ARate KRate -> Element ARate
mac opF = 
    mkElement "mac" inspec (Out1 A)
  where
    inspec = mapOpcode2 opF $ \(a,b) -> 
              [getConfA a, getConfK b]

maca :: OpcodeList1 ARate -> Element ARate
maca opF = 
    mkElement "maca" inspec (Out1 A)
  where
    inspec = mapOpcode opF $ \x -> 
               (getConfA x) 

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

cps2pch :: Opcode2 IRate IRate -> Element IRate
cps2pch opF = 
    mkElement "cps2pch" inspec (Out1 I)
  where
    inspec = applyOpcode opF $ \(ipch, ieq) -> 
               [ getConfI ipch, getConfI ieq ] 


cpsxpch :: Opcode4 IRate IRate IRate IRate 
        -> Element IRate
cpsxpch opF = 
    mkElement "cps2pch" inspec (Out1 I)
  where
    inspec = applyOpcode opF $ \(ipch, ieq, irep, ibase) -> 
               [ getConfI ipch,     getConfI ieq
               , getConfI irep,     getConfI ibase ]
                        
