{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Djembe.GraphicPrimitives
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC 
--
-- Djembe
--
--------------------------------------------------------------------------------

module Wumpus.Djembe.GraphicPrimitives where


import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Basic.System.FontLoader.Base

import Wumpus.Core                              -- package: wumpus-core


import Control.Applicative

type NoteheadWidth u = u


scaleValue :: FromPtSize u => AfmUnit -> DrawingInfo u
scaleValue u1 = fmap (\sz -> afmValue u1 (fromIntegral sz)) getFontSize

-- Measurements are often easier to judge respective to cap height
-- rather than point size.
--
scaleByCapHeight :: (Fractional u, FromPtSize u) 
                 => u -> DrawingInfo u
scaleByCapHeight u1 = (u1 *) <$> scaleValue cap_size


unit_width              :: AfmUnit
unit_width              = 1180

cap_size                :: AfmUnit 
cap_size                = 718

line_height             :: AfmUnit
line_height             = 2545

period_center           :: AfmUnit
period_center           = 108

dot_center              :: AfmUnit
dot_center              = 306

flam_dot_center         :: AfmUnit
flam_dot_center         = 950

stem_top                :: AfmUnit
stem_top                = 2272

stem_length             :: AfmUnit
stem_length             = stem_top - stem_start

stem_start              :: AfmUnit
stem_start              = 818

half_line_pos           :: AfmUnit
half_line_pos           = 2200

flam_xminor             :: AfmUnit
flam_xminor             = 328

flam_baseline           :: AfmUnit
flam_baseline           = 904

flam_stem_length        :: AfmUnit
flam_stem_length        = 636

swing_angle_lower_left  :: AfmUnit
swing_angle_lower_left  = 1600
     


--------------------------------------------------------------------------------
-- Note heads are positioned...


dotNotehead :: (Fractional u, FromPtSize u) => LocImage u (NoteheadWidth u)
dotNotehead = 
    scaleByCapHeight (3/8) >>= \radius -> 
    intoLocImage (pure $ pure $ radius * 2) 
                 (scaleVMove dot_center $ filledDisk radius)


periodNotehead :: (Fractional u, FromPtSize u) => LocImage u (NoteheadWidth u)
periodNotehead = 
    scaleByCapHeight (1/12) >>= \radius -> 
    intoLocImage (pure $ pure $ radius * 3) -- todo
                 (scaleVMove period_center $ filledDisk radius)

letterNotehead :: (Fractional u, FromPtSize u) 
               => AfmUnit -> Char -> LocImage u (NoteheadWidth u)
letterNotehead cw ch = 
    scaleValue cw >>= \char_width -> 
    intoLocImage (pure $ pure char_width) 
                 (hmove (negate $ 0.5*char_width) (textline [ch]))



--
letterFlamGlyph :: (Fractional u, FromPtSize u) 
                => AfmUnit -> Char -> LocGraphic u
letterFlamGlyph cw ch = 
    getFontSize >>= \sz -> 
    localize (fontSize $ (3 * sz) `div` 4) $ 
        let x = negate $ flam_xminor + (0.675 * cw)
            y = flam_baseline
        in scaleMove x y $ textline [ch]

dotFlamGlyph :: (Fractional u, FromPtSize u) => LocGraphic u
dotFlamGlyph = 
    scaleByCapHeight (3/16) >>= \radius -> 
    scaleMove (negate $ flam_xminor) flam_dot_center $ filledDisk radius

--------------------------------------------------------------------------------
-- stems

singleStem :: (Fractional u, FromPtSize u) => LocGraphic u
singleStem =
   scaleValue stem_length >>= \len -> 
   scaleVMove stem_start (straightLine $ vvec len)


flamStem :: (Fractional u, FromPtSize u) => LocGraphic u
flamStem = 
    scaleVecPath flam_path >>= \vs -> 
    scaleVMove stem_start (openStrokePath vs)
  where
    flam_path  = [ vvec stem_length
                 , vec  (negate flam_xminor) (negate flam_xminor)
                 , vvec (negate flam_stem_length)
                 ]


swingStem :: (Fractional u, FromPtSize u) => LocGraphic u
swingStem = singleStem `oplus` swingAngle
    

swingAngle :: (Fractional u, FromPtSize u) => LocGraphic u
swingAngle = 
    scaleVecPath angle_path >>= \vs -> 
    scaleVMove swing_angle_lower_left (openStrokePath vs)
  where
    angle_path = let w = 0.9*flam_xminor in [ vec w w, vec (-w) w ]



openStrokePath :: Num u => [Vec2 u] -> LocGraphic u
openStrokePath vs = promote1 $ \pt -> openStroke $ vectorPath pt vs

--------------------------------------------------------------------------------

makeDjembeNote :: (Fractional u, FromPtSize u) 
               => LocImage u (NoteheadWidth u)
               -> LocImage u (NoteheadWidth u)
makeDjembeNote note_head = superimposeLocImage note_head singleStem 


makeFlamNote :: (Fractional u, FromPtSize u) 
             => LocImage u (NoteheadWidth u) 
             -> LocGraphic u 
             -> LocImage u (NoteheadWidth u)
makeFlamNote note_head flam_head = 
    superimposeLocImage note_head (flam_head `oplus` flamStem) 



superimposeLocImage :: LocImage u a -> LocGraphic u -> LocImage u a
superimposeLocImage img gfx = 
     img `bind1` \(a,b) -> gfx `bind1` \z -> wrap1 (a, b `oplus` z)


hmove :: Num u => u -> LocCF u a -> LocCF u a
hmove x = prepro1 (hdisplace x)

vmove :: Num u => u -> LocCF u a -> LocCF u a
vmove y = prepro1 (hdisplace y)


scaleMove :: FromPtSize u => AfmUnit -> AfmUnit -> LocCF u a -> LocCF u a
scaleMove x y cf = 
    scaleValue x >>= \xu -> scaleValue y >>= \yu -> prepro1 (displace xu yu) cf

scaleHMove :: FromPtSize u => AfmUnit -> LocCF u a -> LocCF u a
scaleHMove x cf = scaleValue x >>= \xu -> prepro1 (hdisplace xu) cf

scaleVMove :: FromPtSize u => AfmUnit -> LocCF u a -> LocCF u a
scaleVMove y cf = scaleValue y >>= \yu -> prepro1 (vdisplace yu) cf


scaleVecPath :: FromPtSize u => [Vec2 AfmUnit] -> DrawingInfo [Vec2 u]
scaleVecPath = mapM scaleVec2 
  where
    scaleVec2 (V2 x y) = V2 <$> scaleValue x <*> scaleValue y


--------------------------------------------------------------------------------





