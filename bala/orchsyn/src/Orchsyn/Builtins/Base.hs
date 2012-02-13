{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Orchsyn.Builtins.Base
-- Copyright   :  (c) Stephen Tetley 2012
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Basic opcodes and functions.
--
--------------------------------------------------------------------------------

module Orchsyn.Builtins.Base
  (

  -- * Output
    printf

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


import Orchsyn.Language.Expr
import Orchsyn.OrchMonad
import Orchsyn.Utils.PrettyExpr

import Prelude hiding ( init, negate, (&&), (||), (^)
                      , abs, exp, log, sqrt
                      , sin, cos, tan, sinh, cosh, tanh
                      , sum, product 
                      )


-- | Note - list of args need to be the same type, use cast where
-- necessary.
--
-- Also, a backslash in the input string will need escaping with 
-- another baskslash to be transmitted to Csound.
--
printf :: Rate rate
       => String -> [Expr rate] -> Opcode0 rate
printf ss es = opcodeStmt0 "printf" (e1 : map uniRate es) 
  where
    e1 = StringE ss



-- | Shocking, but this fools the compiler as @undefined@ is never
-- demanded.
--
init :: Expr IInit -> Opcode1 rate
init ia = opcodeStmt1 "init" [ uniRate ia ] 




divz :: Rate rate
     => Expr IInit -> Expr IInit -> Expr IInit -> Opcode1 rate
divz ia ib isubst = opcodeStmt1 "divz" args 
  where
    args= [ uniRate ia, uniRate ib, uniRate isubst ]
                     
    


--------------------------------------------------------------------------------
-- Arithmetic and logical operators


infixr 3 &&

(&&) :: Expr rate -> Expr rate -> Expr rate
(&&) = liftE2 $ BinE logical_and

infixr 2 ||


(||) :: Expr rate -> Expr rate -> Expr rate
(||) = liftE2 $ BinE logical_or

infixr 8 ^

(^)  :: Expr rate -> Expr rate -> Expr rate
(^)  = liftE2 $ BinE power_of


infixl 7 %

(%) :: Expr rate -> Expr rate -> Expr rate
(%) = liftE2 $ BinE modulus_op


--------------------------------------------------------------------------------
-- Math functions



-- | I or K rate only.
--
int       :: IK_Rate rate
          => Expr rate -> Expr rate
int       = liftE1 $ FunCallE "int"


-- | I or K rate only.
--
frac      :: IK_Rate rate
          => Expr rate -> Expr rate
frac      = liftE1 $ FunCallE "frac"


-- | I or K rate only.
--
powoftwo  :: IK_Rate rate
          => Expr rate -> Expr rate
powoftwo  = liftE1 $ FunCallE "powoftwo"


-- | I or K rate only.
--
logbtwo   :: IK_Rate rate
          => Expr rate -> Expr rate
logbtwo   = liftE1 $ FunCallE "logbtwo"



abs       :: Expr rate -> Expr rate
abs       = liftE1 $ FunCallE "abs"

exp       :: Expr rate -> Expr rate
exp       = liftE1 $ FunCallE "exp"

log       :: Expr rate -> Expr rate
log       = liftE1 $ FunCallE "log"

log10     :: Expr rate -> Expr rate
log10     = liftE1 $ FunCallE "log10"

sqrt      :: Expr rate -> Expr rate
sqrt      = liftE1 $ FunCallE "sqrt"


--------------------------------------------------------------------------------
-- Trig functions

sin       :: Expr rate -> Expr rate
sin       = liftE1 $ FunCallE "sin"

cos       :: Expr rate -> Expr rate
cos       = liftE1 $ FunCallE "cos"

tan       :: Expr rate -> Expr rate
tan       = liftE1 $ FunCallE "tan"


sininv    :: Expr rate -> Expr rate
sininv    = liftE1 $ FunCallE "sininv"

cosinv    :: Expr rate -> Expr rate
cosinv    = liftE1 $ FunCallE "cosinv"

taninv    :: Expr rate -> Expr rate
taninv    = liftE1 $ FunCallE "taninv"

sinh      :: Expr rate -> Expr rate
sinh      = liftE1 $ FunCallE "sinh"

cosh      :: Expr rate -> Expr rate
cosh      = liftE1 $ FunCallE "cosh"

tanh      :: Expr rate -> Expr rate
tanh      = liftE1 $ FunCallE "tanh"



--------------------------------------------------------------------------------
-- Amplitude functions

-- | I or K rate only.
--
dbamp     :: IK_Rate rate
          => Expr rate -> Expr rate
dbamp     = liftE1 $ FunCallE "dbamp"


-- | 
ampdb     :: IK_Rate rate
          => Expr rate -> Expr rate
ampdb     = liftE1 $ FunCallE "ampdb"


--------------------------------------------------------------------------------
-- Random functions

-- | I or K rate only.
--
rnd       :: IK_Rate rate
          => Expr rate -> Expr rate
rnd       = liftE1 $ FunCallE "rnd"
 
-- | I or K rate only.
--
birnd     :: IK_Rate rate
          => Expr rate -> Expr rate
birnd     = liftE1 $ FunCallE "birnd"




--------------------------------------------------------------------------------
-- Opcode equivalents of functions



sum :: [Expr ARate] -> Opcode1 ARate
sum xs = opcodeStmt1 "sum" (map uniRate xs) 


product :: [Expr ARate] -> Opcode1 ARate
product xs = opcodeStmt1 "product" (map uniRate xs) 



pow :: Rate rate
    => Expr rate -> Expr rate -> Opcode1 rate
pow a b = opcodeStmt1 "pow" [ uniRate a, uniRate b ]


taninv2 :: Rate rate 
        => Expr rate -> Expr rate -> Opcode1 rate
taninv2 a b = opcodeStmt1 "taninv2" [ uniRate a, uniRate b ]


mac :: Expr ARate -> Expr KRate -> [(Expr ARate, Expr KRate)]
    -> Opcode1 ARate
mac asig1 ksig1 xs = 
    opcodeStmt1 "mac" args
  where
    args     = uniRate asig1 : uniRate ksig1 : concatMap fn xs
    fn (a,b) = [uniRate a, uniRate b]

maca :: Expr ARate -> Expr ARate -> [Expr ARate] -> Opcode1 ARate
maca asig1 asig2 xs = 
    opcodeStmt1 "maca" args 
  where
    args = uniRate asig1 : uniRate asig2 : map uniRate xs


--------------------------------------------------------------------------------
-- Pitch conversion


-- | Convert a pitch-class value to octave-point-decimal.
-- 
-- I or K rate only.
--
octpch :: IK_Rate rate 
       => Expr rate -> Expr rate
octpch = liftE1 $ FunCallE "octpch"


-- | I or K rate only.
--
cpspch :: IK_Rate rate 
       => Expr rate -> Expr rate
cpspch = liftE1 $ FunCallE "cpspch"

-- | I or K rate only.
--
pchoct :: IK_Rate rate 
       => Expr rate -> Expr rate
pchoct = liftE1 $ FunCallE "pchoct"


-- | I or K rate only.
octcps :: IK_Rate rate 
       => Expr rate -> Expr rate
octcps = liftE1 $ FunCallE "octcps"



-- | No rate restriction.
--
cpsoct    :: Expr rate -> Expr rate
cpsoct    = liftE1 $ FunCallE "cpsoct"


-- Design note - here Csound uses @x@ for the version with more 
-- args. Is this a convention used elsewhere?

cps2pch :: Expr IInit -> Expr IInit -> Opcode1 IInit
cps2pch ipch ieq  = 
    opcodeStmt1 "cps2pch" [ uniRate ipch, uniRate ieq ] 



cpsxpch :: Expr IInit -> Expr IInit -> Expr IInit -> Expr IInit 
        -> Opcode1 IInit
cpsxpch ipch ieq irep ibase = 
    opcodeStmt1 "cps2pch" args
  where
    args = map uniRate [ ipch, ieq, irep, ibase ]
                        

