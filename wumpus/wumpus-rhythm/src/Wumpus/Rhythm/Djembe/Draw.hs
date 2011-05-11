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

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Core                              -- package: wumpus-core

import Data.VectorSpace                         -- package: vector-space

import Control.Applicative
import Control.Monad
import Data.Monoid

--------------------------------------------------------------------------------


-- Syntax

data FlamHead = FlamChar Char
              | FlamDisk
              | FlamNone
  deriving (Eq,Ord,Show)

data NoteHead = NoteChar Char
              | NoteDisk
              | NotePeriod
              | NoteNone


-- | Noteheads get decorated with pressed (diag strike), optional 
-- (parens) and muffled (underscore).
--
-- Noteheads can also have hand hits (tone disk up the stem) and 
-- more.
-- 
-- 
data NoteHeadDeco = NoteHeadDeco 
      { deco_pos_object  :: PosObject AfmUnit -> PosObject AfmUnit 
      , deco_loc_graphic :: LocGraphic AfmUnit
      }

type DNoteHead = (NoteHead, NoteHeadDeco)


data Note = Note   DNoteHead
          | Flam   DNoteHead   FlamHead 
          | Swing  DNoteHead
          | Div    DNoteHead   DNoteHead



zeroDeco :: NoteHeadDeco
zeroDeco = NoteHeadDeco 
      { deco_pos_object  = id 
      , deco_loc_graphic = mempty
      }

decoNoteL :: (PosObject AfmUnit -> PosObject AfmUnit) -> DNoteHead -> DNoteHead
decoNoteL fn (nh,deco) = let f0 = deco_pos_object deco
                         in (nh, deco { deco_pos_object = fn . f0 } )

decoNoteR :: LocGraphic AfmUnit -> DNoteHead -> DNoteHead
decoNoteR gf (nh,deco) = let g0 = deco_loc_graphic deco
                         in (nh, deco { deco_loc_graphic = g0 `mappend` gf } )

optional :: DNoteHead -> DNoteHead
optional = decoNoteL parens

muffled  :: DNoteHead -> DNoteHead
muffled  = decoNoteL underscore

strike   :: DNoteHead -> DNoteHead
strike   = decoNoteL angleStrike





-- Design note - Hi and Lo notes now seem to look like a 
-- decoration, in that a note can also have a hi or lo note.

drawBeamGroups :: [[Note]] -> DjembeDraw ()
drawBeamGroups = mapM_ drawBeamGroup


drawBeamGroup :: [Note] -> DjembeDraw ()
drawBeamGroup []     = return ()
drawBeamGroup (x:xs) = askUnitWidth >>= \uw -> 
    insertStemTop (beamBracket uw (length xs)) >> stepl x >> inner xs
  where
    stepl n      = stem1 LEFT_EXT n >> note1 n >> moveNext

    inner []     = moveNext
    inner [n]    = stem1 RIGHT_EXT n  >> note1 n >> moveNext
    inner (n:ns) = stem1 STEM_INNER n >> note1 n >> moveNext >> inner ns 



stem1 :: StemPos -> Note -> DjembeDraw ()
stem1 pos note = askUnitWidth >>= \uw -> insertStemTop (body note uw)
  where
    body (Note _)       = plainStem pos
    body (Flam _ _)     = flamStem pos
    body (Swing _)      = swingStem pos
    body (Div _ _)      = divStem pos



note1 :: Note -> DjembeDraw ()
note1 note = askUnitWidth >>= \uw -> insertBaselineCenter (body note uw)
  where
    body (Note a)   _        = runPosNoteHead 0 $ dnoteHeadPos a

    body (Flam a b) _        = ga `mappend` gb
      where
        ga    = runPosNoteHead 0 $ dnoteHeadPos a
        gb    = moveStart (dispVec flamv) (flamHead b)
        flamv = go_left flam_xminor ^+^ go_up flam_ynorth

    body (Swing a)  _       = runPosNoteHead flam_xminor $ dnoteHeadPos a

    body (Div a b)  uw      = ga `mappend` gb
      where
        ga    = runPosNoteHead       0  $ dnoteHeadPos a
        gb    = runPosNoteHead (0.5*uw) $ dnoteHeadPos b


dnoteHeadPos :: DNoteHead -> PosNoteHead
dnoteHeadPos (nh, d) = let upd = deco_pos_object d in upd $ step nh
  where
    step (NoteChar ch) = charNote ch
    step NoteDisk      = diskNote
    step NotePeriod    = periodNote
    step NoteNone      = noNote


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


-- | Run a DjembeDraw with the supplied unit width, the result is a 
-- 'LocGraphic'.
--
runDjembeDraw :: InterpretUnit u 
              => UnitWidth -> DjembeDraw a -> LocGraphic u
runDjembeDraw uw mf = post $ runLocTrace $ getDjembeDraw mf uw 
  where
    post (_,_,gf) = localize (relative_line_width fn . join_miter) $ 
                      uconvLocImageF gf
    
    fn sz         = let s1 = (fromIntegral sz) / (10.0::Double)
                        i1 = ceiling s1 `asTypeOf` (1::Integer)
                    in 0.5 * (fromIntegral $ i1)




withFontSize :: (FontSize -> DrawingContextF) -> DrawingContextF
withFontSize fn = (\s i -> fn i s) <*> dc_font_size


-- | Move rightwards by the unit width.
--
moveNext :: DjembeDraw ()
moveNext = DjembeDraw $ \r -> moveBy (hvec r)

-- | Insert a graphic at /baseline center/ - this is the position 
-- for notes.
--
insertBaselineCenter :: LocGraphic AfmUnit -> DjembeDraw ()
insertBaselineCenter gf = DjembeDraw $ \_ -> insertl_ gf

-- | Insert a graphic at /stem top/ - this is the position for 
-- stems and beams (drawn with a pivot).
--
insertStemTop :: LocGraphic AfmUnit -> DjembeDraw ()
insertStemTop gf = DjembeDraw $ \_ -> insertl_ (moveStart mv gf)
  where
    mv = dispV stem_top


asksUnitWidth :: (AfmUnit -> a) -> DjembeDraw a
asksUnitWidth f = DjembeDraw $ \r -> return (f r)

askUnitWidth :: DjembeDraw AfmUnit
askUnitWidth = DjembeDraw $ \r -> return r


