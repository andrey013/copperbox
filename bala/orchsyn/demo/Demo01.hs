{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE NoImplicitPrelude          #-}

module Demo01 where

import Orchsyn.Builtins.Base
import Orchsyn.Builtins.SignalGenerators
import Orchsyn.Builtins.SignalModifiers
import Orchsyn.Language.Expr
import Orchsyn.Language.PrettyPrint
import Orchsyn.Language.PrimAst
import Orchsyn.Language.Prelude
import Orchsyn.OrchMonad

import Orchsyn.Utils.PrettyExpr
import Orchsyn.Utils.PrettyExtras

import Text.PrettyPrint.HughesPJ

e01 = 4 ^ 5

dummy1 = execInstr $ do
  a1 <- newAVar $ tablei (5 :: AR) 1
  return a1

demo01 = ppInstDef $ instrument 4 $ do
   a1 <- newAVar $ init (5 :: IR)
   a1 $= (rval a1) * 0.75
   k1 <- newAVar $ init (6 :: IR)
   return ()


type Signal rate  = Expr rate
type SignalM rate = Instr (Signal rate)

newtype EnvSpec rate = EnvSpec { getEnvSpec :: Signal IInit -> SignalM rate }


-- | Note - we can have literal calls to p4 in the instrument
-- and perform a transformation to recover @idur@ as a decl and 
-- var-use.
--
runEnv :: EnvSpec rate -> SignalM rate
runEnv env = getEnvSpec env $ iexpr $ PfieldE 4

-- | Alternatively we can make a signal transformer out of an 
-- envelope by applying with multiplication.


-- sin (oscil) and noise are definitely signals.


-- Could Instr take an environment of P-fields?
-- Any /vars/ would not be bounds they could


{-

-- | Maybe it would be better for @line@ envelope to take two 
-- Doubles. The only advantage of taking Exprs is that they can 
-- accommodate P-fields (they can also accommodate bad types like 
-- String).
-- 
env_line :: (KA_Rate rate, TypeRate rate) 
         => Expr IInit -> Expr IInit -> EnvSpec rate
env_line v1 v2 = EnvSpec $ \idur -> newKVar $ line v1 idur v2

-}

--
-- All instruments have idur - if it is unused, the compiler
-- should be able to remove it.
-- 
