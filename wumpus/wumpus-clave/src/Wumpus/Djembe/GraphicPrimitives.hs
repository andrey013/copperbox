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



import Data.AffineSpace                         -- package: vector-space

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
period_center           = 273

dot_center              :: AfmUnit
dot_center              = 405

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
flam_baseline           = 600 -- ???

flam_stem_length        :: AfmUnit
flam_stem_length        = 636





baseline :: LocDrawingInfo u (Point2 u)
baseline = pure $ \pt -> pt

displaceBaseline :: Num u => Vec2 u -> LocDrawingInfo u (Point2 u)
displaceBaseline v = pure $ \pt -> pt .+^ v



dispBasePt :: (Fractional u, FromPtSize u) 
           => AfmUnit -> AfmUnit -> LocDrawingInfo u (Point2 u)
dispBasePt ux uy = scaleValue ux >>= \x -> 
                   scaleValue uy >>= \y -> (displaceBaseline $ vec x y) 


vdispBasePt :: (Fractional u, FromPtSize u) 
            => AfmUnit -> LocDrawingInfo u (Point2 u)
vdispBasePt ua = scaleValue ua >>= (displaceBaseline . vvec) 


hdispBasePt :: (Fractional u, FromPtSize u) 
            => AfmUnit -> LocDrawingInfo u (Point2 u)
hdispBasePt ua = scaleValue ua >>= (displaceBaseline . hvec) 


stemStart           :: (Fractional u, FromPtSize u) 
                    => LocDrawingInfo u (Point2 u)
stemStart           = vdispBasePt stem_start

stemTop             :: (Fractional u, FromPtSize u) 
                    => LocDrawingInfo u (Point2 u)
stemTop             = vdispBasePt stem_top


swingBottomLeft     :: (Fractional u, FromPtSize u) 
                    => LocDrawingInfo u (Point2 u)
swingBottomLeft     = 
    scaleValue flam_xminor >>= \xminor -> 
    postpro1 (.+^ vec (0.1*xminor) (negate $ 2 * xminor)) stemTop
 
halfLineLeft        :: (Fractional u, FromPtSize u) 
                    => LocDrawingInfo u (Point2 u)
halfLineLeft        = vdispBasePt half_line_pos


relativeTo :: LocGraphic u -> LocDrawingInfo u (Point2 u) -> LocGraphic u
relativeTo mf upd = promote1 $ \pt -> 
    situ1 upd pt >>= \p2 -> mf `at` p2 

singleStemmed :: (Fractional u, FromPtSize u) => LocGraphic u -> LocGraphic u
singleStemmed g = g `oplus` singleStem




dot :: (Fractional u, FromPtSize u) => LocGraphic u
dot = singleStemmed $ dotNh `relativeTo` vdispBasePt dot_center


flamDot :: (Fractional u, FromPtSize u) => LocGraphic u
flamDot = dotFh `relativeTo` dispBasePt (-flam_xminor) flam_dot_center

fullstop :: (Fractional u, FromPtSize u) => LocGraphic u
fullstop = singleStemmed $ periodNh `relativeTo` vdispBasePt period_center

periodNh :: (Fractional u, FromPtSize u) => LocGraphic u
periodNh = scaleByCapHeight (1/12) >>= filledDisk

letter :: (Fractional u, FromPtSize u) => AfmUnit -> Char -> LocGraphic u
letter cw ch = 
    singleStemmed $ textline [ch] `relativeTo` hdispBasePt (negate $ 0.5*cw)


smallLetter :: (Fractional u, FromPtSize u) => AfmUnit -> Char -> LocGraphic u
smallLetter cw ch = 
    getFontSize >>= \sz -> 
    localize (fontSize $ (3 * sz) `div` 4) $ 
        textline [ch] `relativeTo` hdispBasePt (negate $ 0.5 * 0.75 * cw)





-- radius is 3/8 of cap height.
--
dotNh :: (Fractional u, FromPtSize u) => LocGraphic u
dotNh = scaleByCapHeight (3/8) >>= filledDisk





-- 
dotNotehead :: (Fractional u, FromPtSize u) => LocImage u (NoteheadWidth u)
dotNotehead = 
    scaleByCapHeight (3/8) >>= \radius -> 
    intoLocImage (pure $ pure $ radius * 2) 
                 (scaledVMove dot_center $ filledDisk radius)


periodNotehead :: (Fractional u, FromPtSize u) => LocImage u (NoteheadWidth u)
periodNotehead = 
    scaleByCapHeight (1/12) >>= \radius -> 
    intoLocImage (pure $ pure $ radius * 3) -- todo
                 (scaledVMove period_center $ filledDisk radius)

-- This notehead is positioned...
--

letterNotehead :: (Fractional u, FromPtSize u) 
               => AfmUnit -> Char -> LocImage u (NoteheadWidth u)
letterNotehead cw ch = 
    scaleValue cw >>= \char_width -> 
    intoLocImage (pure $ pure char_width) 
                 (hmove (negate $ 0.5*char_width) (textline [ch]))

-- 0.375 is  half of 3/4 size
--
flamLetter :: (Fractional u, FromPtSize u) => AfmUnit -> Char -> LocGraphic u
flamLetter cw ch = 
    getFontSize >>= \sz -> 
    localize (fontSize $ (3 * sz) `div` 4) $ 
        let x = negate $ flam_xminor + (0.375 * cw)   
            y = flam_baseline
        in scaledMove x y $ textline [ch]



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


scaledMove :: FromPtSize u => AfmUnit -> AfmUnit -> LocCF u a -> LocCF u a
scaledMove x y cf = 
    scaleValue x >>= \xu -> scaleValue y >>= \yu -> prepro1 (displace xu yu) cf

scaledHMove :: FromPtSize u => AfmUnit -> LocCF u a -> LocCF u a
scaledHMove x cf = scaleValue x >>= \xu -> prepro1 (hdisplace xu) cf

scaledVMove :: FromPtSize u => AfmUnit -> LocCF u a -> LocCF u a
scaledVMove y cf = scaleValue y >>= \yu -> prepro1 (vdisplace yu) cf


-- flam heads are just LocGraphics they cannot be 
-- parenthesized to make an optional.

dotFh :: (Fractional u, FromPtSize u) => LocGraphic u
dotFh = scaleByCapHeight (3/16) >>= filledDisk




stemline :: (Fractional u, FromPtSize u) => LocGraphic u
stemline = scaleValue stem_length >>= (straightLine . vvec)

flamPath :: FromPtSize u => DrawingInfo [Vec2 u]
flamPath = (\h minor flam_h -> [ vvec h, vec (-minor) (-minor), vvec (-flam_h) ])
            <$> scaleValue stem_length <*> scaleValue flam_xminor 
                                       <*> scaleValue flam_stem_length



flamStem :: (Fractional u, FromPtSize u) => LocGraphic u
flamStem = body `relativeTo` stemStart
  where  
    body = flamPath >>= openStrokePath
           

swingStem :: (Fractional u, FromPtSize u) => LocGraphic u
swingStem = stalk `oplus` angle          
  where  
    stalk = stemline `relativeTo` stemStart
    angle = (swingAnglePath >>= openStrokePath) `relativeTo` swingBottomLeft

swingAnglePath :: (Fractional u, FromPtSize u) => DrawingInfo [Vec2 u]
swingAnglePath = (\minor -> let w = 0.8*minor in [ vec w w, vec (-w) w ])
                    <$> scaleValue flam_xminor 


-- Note - line thickness should vary according to size...
--
singleStem :: (Fractional u, FromPtSize u) => LocGraphic u
singleStem = stemline `relativeTo` stemStart


openStrokePath :: Num u => [Vec2 u] -> LocGraphic u
openStrokePath vs = promote1 $ \pt -> openStroke $ vectorPath pt vs


