{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Majalan.Basic.Kernel.Objects.Score
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
--
--------------------------------------------------------------------------------

module Majalan.Basic.Kernel.Objects.Score
  ( 

    traceNotelist
  , traceNotelistU

    
  ) where


import Majalan.Basic.Kernel.Base.Context
import Majalan.Basic.Kernel.Base.WrappedPrimitive
import Majalan.Basic.Kernel.Objects.TraceNotelist

import Majalan.Core                             -- package: majalan-core


--
-- Note - because we don\'t load font metrics from file, sharing 
-- context is not so valuable...
--


traceNotelist :: Context ctx -> Notelist ctx u a -> Maybe (Score ctx)
traceNotelist ctx mf = liftToScoreMb $ execNotelist ctx mf


traceNotelistU :: Context ctx -> Notelist ctx u a -> Score ctx
traceNotelistU ctx mf = maybe fk id $ traceNotelist ctx mf
  where
    fk = error "traceNotelistU - emptyScore." 





-- | Promotion of @HPrim@ to @Picture@.
--
-- 
liftToScoreMb :: HPrim ctx u -> Maybe (Score ctx)
liftToScoreMb hf = let prims = hprimToList hf in 
                   if null prims then Nothing else Just (frame prims)

