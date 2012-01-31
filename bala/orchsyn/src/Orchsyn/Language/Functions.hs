{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Orchsyn.Language.Functions
-- Copyright   :  (c) Stephen Tetley 2012
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  unstable
-- Portability :  GHC
--
-- Builtin Csound functions.
--
--------------------------------------------------------------------------------


module Orchsyn.Language.Functions
  (


  -- * Math functions
    int
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


  ) where

import Orchsyn.Language.Expr

import Prelude ( ($), (.) )

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

