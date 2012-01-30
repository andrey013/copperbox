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

import Prelude ()

--------------------------------------------------------------------------------
-- Math functions


-- | I or K rate only.
--
int       :: Expr -> Expr
int       = FunCallE "int"


-- | I or K rate only.
--
frac      :: Expr -> Expr
frac      = FunCallE "frac"


-- | I or K rate only.
--
powoftwo  :: Expr -> Expr   
powoftwo  = FunCallE "powoftwo"


-- | I or K rate only.
--
logbtwo   :: Expr -> Expr
logbtwo   = FunCallE "logbtwo"



abs       :: Expr -> Expr
abs       = FunCallE "abs"

exp       :: Expr -> Expr
exp       = FunCallE "exp"

log       :: Expr -> Expr
log       = FunCallE "log"

log10     :: Expr -> Expr
log10     = FunCallE "log10"

sqrt      :: Expr -> Expr
sqrt      = FunCallE "sqrt"


--------------------------------------------------------------------------------
-- Trig functions

sin       :: Expr -> Expr
sin       = FunCallE "sin"

cos       :: Expr -> Expr
cos       = FunCallE "cos"

tan       :: Expr -> Expr
tan       = FunCallE "tan"


sininv    :: Expr -> Expr
sininv    = FunCallE "sininv"

cosinv    :: Expr -> Expr
cosinv    = FunCallE "cosinv"

taninv    :: Expr -> Expr
taninv    = FunCallE "taninv"

sinh      :: Expr -> Expr
sinh      = FunCallE "sinh"

cosh      :: Expr -> Expr
cosh      = FunCallE "cosh"

tanh      :: Expr -> Expr
tanh      = FunCallE "tanh"

--------------------------------------------------------------------------------
-- Amplitude functions

-- | I or K rate only.
--
dbamp     :: Expr -> Expr
dbamp     = FunCallE "dbamp"


-- | 
ampdb     :: Expr -> Expr
ampdb     = FunCallE "ampdb"

--------------------------------------------------------------------------------
-- Random functions

-- | I or K rate only.
--
rnd       :: Expr -> Expr
rnd       = FunCallE "rnd"
 
-- | I or K rate only.
--
birnd     :: Expr -> Expr
birnd     = FunCallE "birnd"

