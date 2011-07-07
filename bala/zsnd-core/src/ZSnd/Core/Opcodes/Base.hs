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

   -- * Output
--    out1
--  , out2

  -- * Variable initialization
    init
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


import ZSnd.Core.CsoundInst.Prim
import ZSnd.Core.CsoundInst.Typed


import Prelude hiding ( init, negate, (&&), (||), (^)
                      , abs, exp, log, sqrt
                      , sin, cos, tan, sinh, cosh, tanh
                      , sum, product 
                      )




-- binop :: Rator -> Expr rate -> Expr rate -> Expr rate
-- binop op a b = mkExpr $ BinOp op (getExprI $ cast a) (getExprI $ cast b)

-- unop :: Rator -> Expr rate -> Expr rate
-- unop op a = mkExpr $ CUnOp op (getExprI $ cast a)

{-

out1 :: Opcode1 ARate -> Opcode rate
out1 opF = 
    Opcode "out" inspec [] Out0 
  where
    inspec = applyOpcode opF $ \ ain -> 
               [ getExprA ain ]

out2 :: Opcode2 ARate ARate -> Opcode rate
out2 opF = 
    Opcode "outs" inspec [] Out0
  where
    inspec = applyOpcode opF $ \(ain1, ain2) -> 
               [ getExprA ain1, getExprA ain2 ]
-}

init :: forall rate. (Rate rate) 
     => Expr IInit -> Opcode1 rate
init ia = 
    Opcode1 "init" [ getExprI ia ] 




-- tival :: Expr IR
-- tival = Expr $ ZeroOp "tival"

divz :: forall rate. (Rate rate) 
     => Expr IInit -> Expr IInit -> Expr IInit -> Opcode1 rate
divz ia ib isubst =
    Opcode1 "divz" [ getExprI ia, getExprI ib, getExprI isubst ]
                     
    


--------------------------------------------------------------------------------
-- Arithmetic and logical operators



infixr 8 ^
infixr 3 &&
infixr 2 ||



(&&) :: IK_Rate rate
     => Expr rate -> Expr rate -> Expr rate
(&&) = binop logical_and

(||) :: IK_Rate rate 
     => Expr rate -> Expr rate -> Expr rate
(||) = binop logical_or

(^)  :: IK_Rate rate
     => Expr rate -> Expr rate -> Expr rate
(^)  = binop power_of


infixl 7 %

(%) :: Expr rate -> Expr rate -> Expr rate
(%) = binop modulus_op


--------------------------------------------------------------------------------
-- Math functions


-- | I or K rate only.
--
int       :: IK_Rate rate
          => Expr rate -> Expr rate
int       = funcall "int"
  

-- | I or K rate only.
--
frac      :: IK_Rate rate 
          => Expr rate -> Expr rate
frac      = funcall "frac"


-- | I or K rate only.
--
powoftwo  :: IK_Rate rate 
          => Expr rate -> Expr rate   
powoftwo  = funcall "powoftwo"


-- | I or K rate only.
--
logbtwo   :: IK_Rate rate => Expr rate -> Expr rate
logbtwo   = funcall "logbtwo"



abs       :: Expr rate -> Expr rate
abs       = funcall "abs"

exp       :: Expr rate -> Expr rate
exp       = funcall "exp"

log       :: Expr rate -> Expr rate
log       = funcall "log"

log10     :: Expr rate -> Expr rate
log10     = funcall "log10"

sqrt      :: Expr rate -> Expr rate
sqrt      = funcall "sqrt"


--------------------------------------------------------------------------------
-- Trig functions

sin       :: Expr rate -> Expr rate
sin       = funcall "sin"

cos       :: Expr rate -> Expr rate
cos       = funcall "cos"

tan       :: Expr rate -> Expr rate
tan       = funcall "tan"


sininv    :: Expr rate -> Expr rate
sininv    = funcall "sininv"

cosinv    :: Expr rate -> Expr rate
cosinv    = funcall "cosinv"

taninv    :: Expr rate -> Expr rate
taninv    = funcall "taninv"

sinh      :: Expr rate -> Expr rate
sinh      = funcall "sinh"

cosh      :: Expr rate -> Expr rate
cosh      = funcall "cosh"

tanh      :: Expr rate -> Expr rate
tanh      = funcall "tanh"

--------------------------------------------------------------------------------
-- Amplitude functions

-- | I or K rate only.
--
dbamp     :: IK_Rate rate 
          => Expr rate -> Expr rate
dbamp     = funcall "dbamp"


-- | 
ampdb     :: Expr rate -> Expr rate
ampdb     = funcall "ampdb"

--------------------------------------------------------------------------------
-- Random functions

-- | I or K rate only.
--
rnd       :: IK_Rate rate 
          => Expr rate -> Expr rate
rnd       = funcall "rnd"
 
-- | I or K rate only.
--
birnd     :: IK_Rate rate 
          => Expr rate -> Expr rate
birnd     = funcall "birnd"


--------------------------------------------------------------------------------
-- Opcode equivalents of functions


-- Not sure how to handle these...

sum :: [Expr ARate] -> Opcode1 ARate
sum xs = 
    Opcode1 "sum" (map getExprA xs) 


product :: [Expr ARate] -> Opcode1 ARate
product xs = 
    Opcode1 "product" (map getExprA xs) 



pow :: forall rate. (Rate rate) 
    => Expr rate -> Expr rate -> Opcode1 rate
pow a b = 
    Opcode1 "pow" [ getExprUniv a, getExprUniv b ]


taninv2 :: forall rate. (Rate rate) 
        => Expr rate -> Expr rate -> Opcode1 rate
taninv2 a b = 
    Opcode1 "taninv2" [ getExprUniv a, getExprUniv b ]


mac :: Expr ARate -> Expr KRate -> [(Expr ARate, Expr KRate)]
    ->  Opcode1 ARate
mac asig1 ksig1 xs = 
    Opcode1 "mac" (getExprA asig1 : getExprK ksig1 : concatMap fn xs)
  where
    fn (a,b) = [getExprA a, getExprK b]

maca :: Expr ARate -> Expr ARate -> [Expr ARate] -> Opcode1 ARate
maca asig1 asig2 xs = 
    Opcode1 "maca" (getExprA asig1 : getExprA asig2 : map getExprA xs) 

--------------------------------------------------------------------------------
-- Pitch conversion


-- | Convert a pitch-class value to octave-point-decimal.
-- 
-- I or K rate only.
--
octpch :: IK_Rate rate 
       => Expr rate -> Expr rate
octpch = funcall "octpch"


-- | I or K rate only.
--
cpspch :: IK_Rate rate 
       => Expr rate -> Expr rate
cpspch = funcall "cpspch"

-- | I or K rate only.
--
pchoct :: IK_Rate rate 
       => Expr rate -> Expr rate
pchoct = funcall "pchoct"


-- | I or K rate only.
octcps :: IK_Rate rate 
       => Expr rate -> Expr rate
octcps = funcall "octcps"



-- | No rate restriction.
--
cpsoct    :: Expr rate -> Expr rate
cpsoct    = funcall "cpsoct"


-- Design note - here Csound uses @x@ for the version with more 
-- args. Is this a convention used elsewhere?

cps2pch :: Expr IInit -> Expr IInit -> Opcode1 IInit
cps2pch ipch ieq  = 
    Opcode1 "cps2pch" [ getExprI ipch, getExprI ieq ] 



cpsxpch :: Expr IInit -> Expr IInit -> Expr IInit -> Expr IInit 
        -> Opcode1 IInit
cpsxpch ipch ieq irep ibase = 
    Opcode1 "cps2pch" [ getExprI ipch,     getExprI ieq
                      , getExprI irep,     getExprI ibase ]
                        
