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

-- import Wumpus.Drawing.Basis.LocTrace            -- package: wumpus-drawing

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Core                              -- package: wumpus-core

import Data.VectorSpace                         -- package: vector-space

import Control.Applicative
import Control.Monad
import Data.Monoid

--------------------------------------------------------------------------------


-- Syntax

type FlamHead = LocGraphic AfmUnit


data NoteHead = NoteHead 
      { notehead_base   :: PosNoteHead
      , notehead_trafo  :: PosGraphic AfmUnit -> PosGraphic AfmUnit
      }
                        

noteHead :: PosNoteHead -> NoteHead
noteHead po = NoteHead { notehead_base   = po
                       , notehead_trafo  = id
                       }

-- | Legend:
--
-- > N - note
-- > F - flam
-- > S - swing
-- > D - division of beat into two
--
data Note = N NoteHead              Anno
          | F FlamHead   NoteHead   Anno
          | S NoteHead              Anno
          | D NoteHead   NoteHead   Anno

-- TODO - Accents are probably correct as Note embellishments
-- but this makes constructing syntax in client code more 
-- cumbersome...

type Anno = DjembeDraw ()

noAnno :: Anno
noAnno = return ()

notes :: [NoteHead] -> [Note]
notes = map note

note :: NoteHead -> Note 
note a = N a noAnno

flam :: FlamHead -> NoteHead -> Note
flam fh nh = F fh nh noAnno

swing :: NoteHead -> Note
swing a = S a noAnno

-- what to call div? 

addTrafo :: (PosGraphic AfmUnit -> PosGraphic AfmUnit) -> NoteHead -> NoteHead
addTrafo fn = (\s i -> s { notehead_trafo = i `mappend` fn })
                 <*> notehead_trafo


addAccent :: Accent -> Note -> Note
addAccent a (N nh an)       = N nh (an >> accent a)
addAccent a (F fh nh an)    = F fh nh (an >> accent a)
addAccent a (S nh an)       = S nh (an >> accent a)
addAccent a (D n1 n2 an)    = D n1 n2 (an >> accent a)


optional :: NoteHead -> NoteHead
optional = addTrafo parenthesis

leadin :: Note -> Note 
leadin = addAccent leadinAccent


accent :: Accent -> DjembeDraw ()
accent (StemAccent gf)          = insertStemTop gf
accent (BaselineAccent gf)      = insertBLC gf


-- Design note - Hi and Lo notes now seem to look like a 
-- decoration, in that a note can also have a hi or lo note.

drawBeamGroups :: [[Note]] -> DjembeDraw ()
drawBeamGroups = mapM_ beamGroup


beamGroup :: [Note] -> DjembeDraw ()
beamGroup []     = return ()
beamGroup (x:xs) = askUnitWidth >>= \uw -> 
    insertStemTop (beamBracket uw (length xs)) >> stepl x >> inner xs
  where
    stepl n      = stem1 LEFT_EXT n >> note1 n >> moveNext

    inner []     = moveNext
    inner [n]    = stem1 RIGHT_EXT n  >> note1 n >> moveNext
    inner (n:ns) = stem1 STEM_INNER n >> note1 n >> moveNext >> inner ns 



stem1 :: StemPos -> Note -> DjembeDraw ()
stem1 pos nt = askUnitWidth >>= \uw -> insertStemTop (body nt uw)
  where
    body (N _ _)    = plainStem pos
    body (F _ _ _)  = flamStem pos
    body (S _ _)    = swingStem pos
    body (D _ _ _)  = divStem pos



note1 :: Note -> DjembeDraw ()
note1 nt = askUnitWidth >>= \uw -> body nt uw
  where
    body (N a ma)   _      = ma >> insertBLC (runPosNoteHead 0 $ dnoteHeadPos a)

    body (F a b ma) _      = ma >> insertBLC (ga `mappend` gb)
      where
        ga    = moveStart flamv a
        gb    = runPosNoteHead 0 $ dnoteHeadPos b
        flamv = go_left flam_xdist ^+^ go_up flam_ydist

    body (S a ma)  _       = ma >> insertBLC (runPosNoteHead flam_xdist $ dnoteHeadPos a)

    body (D a b ma)  uw    = ma >> insertBLC (ga `mappend` gb)
      where
        ga    = runPosNoteHead       0  $ dnoteHeadPos a
        gb    = runPosNoteHead (0.5*uw) $ dnoteHeadPos b


-- drawAccents :: NoteHead -> DjembeDraw ()
-- drawAccents (NoteHead _ _) = mapM_ accent xs

dnoteHeadPos :: NoteHead -> PosNoteHead
dnoteHeadPos (NoteHead nt trafo) = trafo nt 



barline :: DjembeDraw ()
barline = insertBLC singleBarline >> moveNext

inrepeat :: DjembeDraw a -> DjembeDraw ()
inrepeat ma = lrepeat >> ma >> rrepeat

inbar :: DjembeDraw a -> DjembeDraw ()
inbar ma = barline >> ma >> barline



lrepeat :: DjembeDraw ()
lrepeat = insertBLC leftRepeat >> moveNext

rrepeat :: DjembeDraw ()
rrepeat = insertBLC rightRepeat >> moveNext



--------------------------------------------------------------------------------
-- Drawing Monad

-- Reader (unit width) plus LocTrace.

type UnitWidth = AfmUnit


newtype DjembeDraw a = DjembeDraw { 
      getDjembeDraw :: UnitWidth -> LocTrace UnitWidth a }



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
runDjembeDraw uw ma = 
    uconvF $ runLocTrace_ $ localize upd $ getDjembeDraw ma uw
  where
    upd =  contextual_line_width fn . join_miter
    
    fn sz         = let s1 = (fromIntegral sz) / (10.0::Double)
                        i1 = ceiling s1 `asTypeOf` (1::Integer)
                    in 0.5 * (fromIntegral $ i1)



withFontSize :: (FontSize -> DrawingContextF) -> DrawingContextF
withFontSize fn = (\s i -> fn i s) <*> dc_font_size


-- | Move rightwards by the unit width.
--
moveNext :: DjembeDraw ()
moveNext = DjembeDraw $ \r -> moveby (hvec r)

-- | Insert a graphic at /baseline center/ - this is the position 
-- for notes.
--
insertBLC :: LocGraphic AfmUnit -> DjembeDraw ()
insertBLC gf = DjembeDraw $ \_ -> insertl_ gf >> return ()

-- | Insert a graphic at /stem top/ - this is the position for 
-- stems and beams (drawn with a pivot).
--
insertStemTop :: LocGraphic AfmUnit -> DjembeDraw ()
insertStemTop gf = DjembeDraw $ \_ -> 
    insertl_ (moveStart (vvec stem_top) gf) >> return ()


asksUnitWidth :: (AfmUnit -> a) -> DjembeDraw a
asksUnitWidth f = DjembeDraw $ \r -> return (f r)

askUnitWidth :: DjembeDraw AfmUnit
askUnitWidth = DjembeDraw $ \r -> return r


