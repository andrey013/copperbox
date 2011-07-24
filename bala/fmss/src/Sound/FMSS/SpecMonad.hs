{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Sound.FMSS.SpecMonad
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Low-level abstract syntax
--
--------------------------------------------------------------------------------

module Sound.FMSS.SpecMonad
  (

    Spec
  , SpecAns  
  , Params(..)

  , execSpec

  , var
  , envelope
  , config  

  ) where


import Sound.FMSS.AbstractSyntax
import Sound.FMSS.ConfigMonad
import Sound.FMSS.Datatypes
import Sound.FMSS.Envelopes
import Sound.FMSS.Utils.HList

import Control.Applicative
import Data.Monoid




data Params = Params { instr_num :: Int, sine_table :: Int } 


data SpecAns = SpecAns { ans_body :: SynthBody, ans_out :: Stmt }

-- Note - potentially this can be implemented as an inner
-- /connection spec/. With sharing for envelopes in an outer
-- monad.



newtype W = W { w_decls :: H Decl }

instance Monoid W where
  mempty            = W emptyH
  W a `mappend` W b = W $ a `appendH` b

newtype Spec a = Spec { getSpec :: (a, W) }

instance Functor Spec where
  fmap f mf = Spec $ let (a,w) = getSpec mf in (f a, w)

instance Applicative Spec where
  pure a    = Spec $ (a,mempty)
  mf <*> ma = Spec $ let (f,w1) = getSpec mf
                         (a,w2) = getSpec ma
                     in (f a, w1 `mappend` w2) 

instance Monad Spec where
  return a  = Spec $ (a,mempty)
  ma >>= k  = Spec $ let (a,w1) = getSpec ma
                         (b,w2) = getSpec (k a)
                     in (b, w1 `mappend` w2)


execSpec :: Params -> Spec SpecAns -> FMSynth
execSpec (Params {instr_num = i, sine_table = tnum }) mf = 
    post $ getSpec mf
  where
    post (SpecAns {ans_body = body, ans_out = out}, w) = 
        FMSynth { fm_instr_num  = i
                , fm_sinetbl    = tnum
                , fm_decls      = toListH $ w_decls w
                , fm_synth_body = body 
                , fm_out        = out
                }


var :: String -> Expr -> Spec Expr
var name expr = Spec $ (VarE name, W $ wrapH $ Decl name expr)

                         

envelope :: DeconsEnvelope env => String -> env -> Spec Expr
envelope name env = Spec $ 
    let (opcode,body) = deconsEnvelope env
    in (VarE name, W $ wrapH $ Envelope name opcode body)


config :: (Config o) -> (o -> Expr) -> Spec SpecAns
config cf k = Spec $ 
    let (o,body) = runConfig cf
        outexpr   = k o
    in (SpecAns {ans_body = body, ans_out = Out outexpr}, mempty)
