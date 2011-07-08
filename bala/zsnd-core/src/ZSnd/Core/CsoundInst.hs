{-# LANGUAGE EmptyDataDecls             #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  ZSnd.Core.CsoundInst
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Shim module for the instrument code.
--
--------------------------------------------------------------------------------

module ZSnd.Core.CsoundInst
  (

    module ZSnd.Core.CsoundInst.Monadic
  , module ZSnd.Core.CsoundInst.Typed

  , Orch        -- re-export
  , OrchHeader(..)
  , CsValue(..)

  , default_mono_header
  , default_stereo_header

  ) where

import ZSnd.Core.CsoundInst.Monadic
import ZSnd.Core.CsoundInst.Prim
import ZSnd.Core.CsoundInst.Typed


      
default_mono_header :: OrchHeader 
default_mono_header =  
    OrchHeader { zero_dbfs                = 32767
               , kr_ctrl_rate             = 4410
               , sr_aud_sample_rate       = 44100
               , ksmps_ctrl_period_samps  = 32
               , nchnls_num_chans         = 1
               , seed_gbl_random          = Nothing
               }

default_stereo_header :: OrchHeader 
default_stereo_header =  
    OrchHeader { zero_dbfs                = 32767
               , kr_ctrl_rate             = 4410
               , sr_aud_sample_rate       = 44100
               , ksmps_ctrl_period_samps  = 32
               , nchnls_num_chans         = 2
               , seed_gbl_random          = Nothing
               }
