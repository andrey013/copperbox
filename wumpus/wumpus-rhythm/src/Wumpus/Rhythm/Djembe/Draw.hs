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
import Wumpus.Rhythm.Djembe.Parameters

import Wumpus.Drawing.Paths.Relative            -- package: wumpus-drawing

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Core                              -- package: wumpus-core

import Control.Monad

--------------------------------------------------------------------------------


data Note = Note   NoteHead
          | HiNote NoteHead 
          | Flam   NoteHead               -- add little flam ...
          | Swing  NoteHead
          | Div    NoteHead   NoteHead




-- | Start point is top-left.
--
-- TODO - the outline path is inefficient, Wumpus-Drawing needs
-- optimizing...
-- 
locBeamGroup :: InterpretUnit u => AfmUnit -> Int -> LocGraphic u 
locBeamGroup uw i 
    | i <= 0    = emptyLocGraphic
    | i == 1    = uconvLocImageF $ beam_one
    | otherwise = uconvLocImageF $ execPivot pathl pathr
  where
    beam_one  = locStraightLine (vvec $ negate stem_length)

    pathl     = vline stem_length 
    pathr     = replicateM_ (i-2) (hline uw >> insertl beam_one)
              >> hline uw 
              >> vline (-stem_length)




drawBeamGroup :: (Floating u, InterpretUnit u) => u -> [Note] -> LocGraphic u
drawBeamGroup _  []     = emptyLocGraphic
drawBeamGroup uw (x:xs) = 
    uconvertCtx1 uw >>= \wa -> 
    elems `oplus` uconvLocImageF (beamBracket wa (length xs))
  where
    elems       = distribH uw $ leftExt uw x : step xs
    step []     = []
    step [b]    = [rightExt uw b]
    step (b:bs) = inner uw b : step bs
    



leftExt :: (Fractional u, InterpretUnit u) => u -> Note -> LocGraphic u
leftExt _ (Note a) = 
    runNoteHead a `oplus` runStem singleStemX

leftExt _ (HiNote a) = 
    runNoteHead a `oplus` runStem singleStemX

leftExt _ (Flam a) = 
    runNoteHead a `oplus` runStem flamStemX

leftExt _ (Swing a) = 
    runNoteHead a `oplus` runStem swingStemLX

leftExt w (Div a b) = 
    uconvertCtx1 w >>= \wa -> 
    let n1 = runNoteHead a
        n2 = moveStart (dispH $ 0.5*w) $ runNoteHead b
    in n1 `oplus` n2 `oplus` runStem (divStemLX wa)


inner :: (Fractional u, InterpretUnit u) => u -> Note -> LocGraphic u
inner _ (Note a) = 
    runNoteHead a `oplus` runStem singleStem

inner _ (HiNote a) = 
    runNoteHead a `oplus` runStem singleStem

inner _ (Flam a) = 
    runNoteHead a `oplus` runStem flamStem

inner _ (Swing a) = 
    runNoteHead a `oplus` runStem swingStem

inner w (Div a b) = 
    uconvertCtx1 w >>= \wa -> 
    let n1 = runNoteHead a
        n2 = moveStart (dispH $ 0.5*w) $ runNoteHead b
    in n1 `oplus` n2 `oplus` runStem (divStem wa)

rightExt :: (Fractional u, InterpretUnit u) => u -> Note -> LocGraphic u
rightExt _ (Note a) = 
    runNoteHead a `oplus` runStem singleStemX

rightExt _ (HiNote a) = 
    runNoteHead a `oplus` runStem singleStemX

rightExt _ (Flam a) = 
    runNoteHead a `oplus` runStem flamStemX

rightExt _ (Swing a) = 
    uconvertCtx1 flam_xminor >>= \wa -> 
    moveStart (dispH wa) (runNoteHead a) `oplus` runStem swingStemRX

rightExt w (Div a b) = 
    uconvertCtx1 w >>= \wa -> 
    let n1 = runNoteHead a
        n2 = moveStart (dispH $ 0.5*w) $ runNoteHead b
    in n1 `oplus` n2 `oplus` runStem (divStemRX wa)


{-

-- OLD 

-- Note - Assembling notes within a beam group can be an 
-- \"even distribution\" or a chain.
--
-- Assembling beam groups, barlines etc. looks like a job for
-- 'advances'.
-- 


beamgroup :: (Fractional u, InterpretUnit u) => [LocGraphic u] -> AdvGraphic u
beamgroup [] = pushR1 (replaceAns (hvec 0)) $ emptyLocGraphic
beamgroup gs = 
    unitWidth >>= \w1 -> 
    pushR1 (replaceAns (hvec $ w1 * fromIntegral len)) $ 
      body w1 `decorateR1` beamline (len-1) w1
  where
    len  = length gs
    body = \w1 -> chain_ (chainH w1) gs


-- | point is base line
beamline :: InterpretUnit u => Int -> u -> LocGraphic u
beamline n w1 = uconvLocImageF $ 
    uconvertCtx1 w1 >>= \w1u -> beam_line n w1u


tone :: InterpretUnit u => LocGraphic u
tone = uconvLocImageF $ tone_body

bassB :: InterpretUnit u => LocGraphic u
bassB = uconvLocImageF $ bassB_body

rest :: InterpretUnit u => LocGraphic u
rest = uconvLocImageF $ single_stem

period :: InterpretUnit u => LocGraphic u
period = uconvLocImageF $ locGraphic_ period_note `oplus` single_stem

loTone :: InterpretUnit u => LocGraphic u
loTone = uconvLocImageF $ locGraphic_ lostroke_disk `oplus` single_stem


tone_body :: LocGraphic AfmUnit
tone_body = locGraphic_ disk_note `oplus` single_stem

bassB_body :: LocGraphic AfmUnit
bassB_body = single_stem `oplus` locGraphic_ (char_note 'B')


toneFlam :: InterpretUnit u =>  LocGraphic u
toneFlam = uconvLocImageF $ 
    locGraphic_ disk_note `oplus` disk_flam `oplus` flam_stem

flamgd :: InterpretUnit u =>  LocGraphic u
flamgd = uconvLocImageF $ 
    locGraphic_ (char_note 'd') `oplus` char_flam 'g' `oplus` flam_stem



tick1 :: InterpretUnit u =>  LocGraphic u
tick1 = uconvLocImageF $ 
    tick `oplus` moveStart (displaceH 1000) tick 
         `oplus` moveStart (displaceH 2000) tick
  where
    tick = locStraightLine (vvec (1000::AfmUnit))


unitWidth :: InterpretUnit u => Query u
unitWidth = uconvertCtx1 (1360::AfmUnit)

-}