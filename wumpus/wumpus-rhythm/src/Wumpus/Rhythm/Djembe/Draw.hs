{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Rhythm.Djembe.Draw
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- Djembe drawing primitives
--
--------------------------------------------------------------------------------

module Wumpus.Rhythm.Djembe.Draw where


import Wumpus.Rhythm.Djembe.GraphicPrimitives

import Wumpus.Drawing.Chains                    -- package: wumpus-drawing
import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Core                              -- package: wumpus-core


--------------------------------------------------------------------------------


-- assembling the notes should be a chain...


beamgroup :: (Fractional u, InterpretUnit u) => [LocGraphic u] -> AdvGraphic u
beamgroup [] = replaceAns (hvec 0) $ emptyLocGraphic
beamgroup gs = 
    lift0R1 unitWidth >>= \w1 -> 
    lift0R1 (cvt w1)  >>= \w1u  ->
    replaceAns (hvec (w1u * fromIntegral len)) $ 
      chainH w1u gs `decorate` beamline (len-1) w1u
  where
    len = length gs


-- | point is base line
beamline :: InterpretUnit u => Int -> u -> LocGraphic u
beamline n w1 = uconvertLocImg $ 
    lift0R1 (cvt w1) >>= \w1u -> beam_line n w1u

-- Note - conversion in context needs sorting out in 
-- Wumpus-Basic.
--
cvt :: (InterpretUnit u, InterpretUnit u1) => u -> Query u1
cvt a = normalizeDC a >>= dinterpDC


tone :: InterpretUnit u => LocGraphic u
tone = uconvertLocImg $ tone_body

bassB :: InterpretUnit u => LocGraphic u
bassB = uconvertLocImg $ bassB_body

rest :: InterpretUnit u => LocGraphic u
rest = uconvertLocImg $ single_stem

period :: InterpretUnit u => LocGraphic u
period = uconvertLocImg $ ignoreAns period_note `oplus` single_stem

loTone :: InterpretUnit u => LocGraphic u
loTone = uconvertLocImg $ ignoreAns lostroke_disk `oplus` single_stem


tone_body :: LocGraphic AfmUnit
tone_body = ignoreAns disk_note `oplus` single_stem

bassB_body :: LocGraphic AfmUnit
bassB_body = single_stem `oplus` ignoreAns (char_note 'B')


toneFlam :: InterpretUnit u =>  LocGraphic u
toneFlam = uconvertLocImg $ 
    ignoreAns disk_note `oplus` disk_flam `oplus` flam_stem

flamgd :: InterpretUnit u =>  LocGraphic u
flamgd = uconvertLocImg $ 
    ignoreAns (char_note 'd') `oplus` char_flam 'g' `oplus` flam_stem



tick1 :: InterpretUnit u =>  LocGraphic u
tick1 = uconvertLocImg $ 
    tick `oplus` moveStart (displaceH 1000) tick 
         `oplus` moveStart (displaceH 2000) tick
  where
    tick = locStraightLine (vvec (1000::AfmUnit))


unitWidth :: Query AfmUnit
unitWidth =  normalizeDC (1360::AfmUnit) >>= dinterpDC
