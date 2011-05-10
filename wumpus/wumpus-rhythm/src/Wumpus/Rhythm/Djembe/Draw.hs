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

import Wumpus.Drawing.Basis.TraceLocGraphic     -- package: wumpus-drawing
import Wumpus.Drawing.Paths.Relative       

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Core                              -- package: wumpus-core

import Data.VectorSpace                         -- package: vector-space

import Control.Applicative
import Control.Monad
import Data.Monoid

--------------------------------------------------------------------------------


data FlamHead = FlamChar Char
              | FlamDisk
              | FlamNone
  deriving (Eq,Ord,Show)

data NoteHead = NoteChar Char
              | NoteDisk
              | NotePeriod
              | NoteNone


data Note = Note   NoteHead
          | Flam   NoteHead   FlamHead 
          | Swing  NoteHead
          | Div    NoteHead   NoteHead


-- Design note - Hi and Lo notes now seem to look like a 
-- decoration, in that a note can also have a hi or lo note.




drawBeamGroup :: [Note] -> DjembeDraw ()
drawBeamGroup []     = return ()
drawBeamGroup (_:xs) = 
    askUnitWidth >>= \uw -> insertl (beamBracket uw (length xs))
    



stem1 :: StemPos -> Note -> DjembeDraw ()
stem1 pos note = askUnitWidth >>= \uw -> insertl (body note uw)
  where
    body (Note _)       = plainStem pos
    body (Flam _ _)     = flamStem pos
    body (Swing _)      = swingStem pos
    body (Div _ _)      = divStem pos



note1 :: Note -> DjembeDraw ()
note1 note = askUnitWidth >>= \uw -> insertl (body note uw)
  where
    body (Note a)   _        = runPosNoteHead 0 $ noteHeadPos a

    body (Flam a b) _        = ga `mappend` gb
      where
        ga    = runPosNoteHead 0 $ noteHeadPos a
        gb    = moveStart (dispVec flamv) (flamHead b)
        flamv = go_left flam_xminor ^+^ go_up flam_ynorth

    body (Swing a)  _       = runPosNoteHead flam_xminor $ noteHeadPos a

    body (Div a b)  uw      = ga `mappend` gb
      where
        ga    = runPosNoteHead 0 $ noteHeadPos a
        gb    = runPosNoteHead (0.5*uw) $ noteHeadPos b


noteHeadPos :: NoteHead -> PosNoteHead
noteHeadPos (NoteChar ch)   = charNote ch
noteHeadPos NoteDisk        = diskNote
noteHeadPos NotePeriod      = periodNote
noteHeadPos NoteNone        = noNote


flamHead :: FlamHead -> LocGraphic AfmUnit
flamHead (FlamChar ch)      = charFlam ch
flamHead FlamDisk           = diskFlam
flamHead FlamNone           = mempty



--------------------------------------------------------------------------------
-- Drawing Monad

-- Reader (unit width) plus LocTrace.

type UnitWidth = AfmUnit


newtype DjembeDraw a = DjembeDraw { 
      getDjembeDraw :: UnitWidth -> LocTrace UnitWidth a }

type instance MonUnit (DjembeDraw a) = UnitWidth


-- Functor

instance Functor DjembeDraw where
  fmap f mf = DjembeDraw $ \r -> 
                getDjembeDraw mf r >>= \a -> return (f a)


-- Applicative
                                
instance Applicative DjembeDraw where
  pure a    = DjembeDraw $ \_ -> return a
  mf <*> ma = DjembeDraw $ \r -> 
                getDjembeDraw mf r >>= \f ->
                getDjembeDraw ma r >>= \a ->
                return (f a)

-- Monad

instance Monad DjembeDraw where
  return a  = DjembeDraw $ \_ -> return a
  ma >>= k  = DjembeDraw $ \r -> 
                getDjembeDraw ma r >>= \a -> (getDjembeDraw . k) a r



instance LocTraceM DjembeDraw where
  insertl a = DjembeDraw $ \_ -> insertl a
  location  = DjembeDraw $ \_ -> location 
  moveBy v  = DjembeDraw $ \_ -> moveBy v


asksUnitWidth :: (AfmUnit -> a) -> DjembeDraw a
asksUnitWidth f = DjembeDraw $ \r -> return (f r)

askUnitWidth :: DjembeDraw AfmUnit
askUnitWidth = DjembeDraw $ \r -> return r



runDjembeDraw :: InterpretUnit u => UnitWidth -> DjembeDraw a -> LocGraphic u
runDjembeDraw uw mf = post $ runLocTrace $ getDjembeDraw mf uw 
  where
    post (_,_,gf) = uconvLocImageF gf
