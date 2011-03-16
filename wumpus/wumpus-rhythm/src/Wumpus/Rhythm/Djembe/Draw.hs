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

import Wumpus.Drawing.Chains

import Wumpus.Basic.Kernel                      
import Wumpus.Basic.System.FontLoader.Base.Datatypes

import Wumpus.Core                              -- package: wumpus-core

import Control.Applicative

--------------------------------------------------------------------------------


-- assembling the notes should be a chain...


beamgroup :: (Fractional u, InterpretUnit u) => [LocGraphic u] -> AdvGraphic u
beamgroup [] = replaceAns (hvec 0) $ emptyLocGraphic
beamgroup gs = 
    unitWidth         &=> \w1 -> 
    cvt w1            &=> \w1u  ->
    replaceAns (hvec (w1u * fromIntegral len)) $ 
      chainH w1u gs `decorate` beamline (len-1) w1u
  where
    len = length gs


-- | point is base line
beamline :: InterpretUnit u => Int -> u -> LocGraphic u
beamline n w1 = uconvert $ cvt w1 &=> \w1u -> beam_line n w1u

-- Note - conversion in context needs sorting out in 
-- Wumpus-Basic.
--
cvt :: (InterpretUnit u, InterpretUnit u1) => u -> Query u1
cvt a = normalizeDC a >>= dinterpDC


tone :: InterpretUnit u => LocGraphic u
tone = uconvert $ tone_body

bassB :: InterpretUnit u => LocGraphic u
bassB = uconvert $ bassB_body

rest :: InterpretUnit u => LocGraphic u
rest = uconvert $ single_stem

period :: InterpretUnit u => LocGraphic u
period = uconvert $ ignoreAns period_note `oplus` single_stem

loTone :: InterpretUnit u => LocGraphic u
loTone = uconvert $ ignoreAns lostroke_disk `oplus` single_stem


tone_body :: LocGraphic AfmUnit
tone_body = ignoreAns disk_note `oplus` single_stem

bassB_body :: LocGraphic AfmUnit
bassB_body = single_stem `oplus` ignoreAns (char_note 'B')


toneFlam :: InterpretUnit u =>  LocGraphic u
toneFlam = uconvert $ 
    ignoreAns disk_note `oplus` disk_flam `oplus` flam_stem

flamgd :: InterpretUnit u =>  LocGraphic u
flamgd = uconvert $ 
    ignoreAns (char_note 'd') `oplus` char_flam 'g' `oplus` flam_stem



tick1 :: InterpretUnit u =>  LocGraphic u
tick1 = uconvert $ 
    tick `oplus` moveStart (displaceH 1000) tick `oplus` moveStart (displaceH 2000) tick
  where
    tick = locStraightLine (vvec (1000::AfmUnit))


unitWidth :: Query AfmUnit
unitWidth =  normalizeDC (1360::AfmUnit) >>= dinterpDC

{-

-- unit width for 16 beats is 1380 Afm



-- | Compute the size of a measurement in Afm units scaled by the
-- point size of the font.
--
afmUnit :: InterpretUnit u => AfmUnit -> DrawingInfo u
afmUnit u = (afmValue u . fromIntegral) <$> getFontSize




-- | Functorial scaling / conversion function.
--
-- This is the one for Graphic - although a Graphic has
-- no useful answer the actually answer still has a unit, i.e. 
-- @UNil u@.
--
scaleFromAfm :: (Functor t, InterpretUnit u) 
               => LocImage AfmUnit (t AfmUnit) -> LocImage u (t u)
scaleFromAfm obj = getFontSize >>= \sz -> 
                 let up   = (* (fromIntegral sz))
                     f1   = fromPsPoint . toPsPoint
                     f2   = fromPsPoint . up . toPsPoint
                     conv = convertli $ ConvertAlg f1 f2 (fmap f2)
                 in conv obj

-- TODO - what should the above do about the unit (scaling) of
-- the start point. Adding the code below produces results that
-- look wrong, but I have to think some more about why they look
-- wrong... 
-- 
-- >                   down = (\u -> u / (fromIntegral sz))
-- >                   f1   = fromPsPoint . down . toPsPoint


               

uToAfm :: InterpretUnit u => u -> AfmUnit
uToAfm = fromPsPoint . toPsPoint

afmToU :: InterpretUnit u => AfmUnit -> u
afmToU = fromPsPoint . toPsPoint




-- | Functorial scaling / conversion function.
--
-- This is the one for Graphic - although a Graphic has
-- no useful answer the actually answer still has a unit, i.e. 
-- @UNil u@.
--
scaleImage :: (Functor t, InterpretUnit u) 
           => Image t AfmUnit -> Image u (t u)
scaleImage obj = uconvert obj


-}