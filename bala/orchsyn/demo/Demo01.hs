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
  a1 :: AR <- tablei (5 :: AR) 1
  return a1

demo01 = ppInstDef $ instrument 4 $ do
   a1 :: AR <- init (5 :: IR)
   k1 :: KR <- init (6 :: IR)
   return ()

newtype EnvSpec rate = EnvSpec { getEnvSpec :: Expr IInit -> Instr (Expr rate) }

env_line :: (KA_Rate rate, MakeVar rate, TypeRate rate) 
         => Expr IInit -> Expr IInit -> EnvSpec rate
env_line v1 v2 = EnvSpec $ \idur -> line v1 idur v2

--
-- All instruments have idur - if it is unused, the compiler
-- should be able to remove it.
-- 
