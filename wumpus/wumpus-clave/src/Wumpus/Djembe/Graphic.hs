{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Djembe.Graphic
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

module Wumpus.Djembe.Graphic where

import Wumpus.Core                              -- package: wumpus-core

import Wumpus.Basic.FontLoader.Base
import Wumpus.Basic.Graphic                     -- package: wumpus-basic
-- import Wumpus.Basic.SafeFonts 

import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace

import Control.Applicative



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

stem_length             :: AfmUnit
stem_length             = 1454

stem_start              :: AfmUnit
stem_start              = 818

flam_xminor             :: AfmUnit
flam_xminor             = 328

flam_stem_length        :: AfmUnit
flam_stem_length        = 636





baseline :: LocDrawingInfo u (Point2 u)
baseline = pure $ \pt -> pt

displaceBaseline :: Num u => Vec2 u -> LocDrawingInfo u (Point2 u)
displaceBaseline v = pure $ \pt -> pt .+^ v




vdispBasePt :: (Fractional u, FromPtSize u) 
            => AfmUnit -> LocDrawingInfo u (Point2 u)
vdispBasePt ua = scaleValue ua >>= (displaceBaseline . vvec) 



dotCenterline       :: (Fractional u, FromPtSize u) 
                    => LocDrawingInfo u (Point2 u)
dotCenterline       = vdispBasePt dot_center



periodCenterline    :: (Fractional u, FromPtSize u) 
                    => LocDrawingInfo u (Point2 u)
periodCenterline    = vdispBasePt period_center

stemStart           :: (Fractional u, FromPtSize u) 
                    => LocDrawingInfo u (Point2 u)
stemStart           = vdispBasePt stem_start

stemTop             :: (Fractional u, FromPtSize u) 
                    => LocDrawingInfo u (Point2 u)
stemTop             = vdispBasePt (stem_start + stem_length)



relativeTo :: LocGraphic u -> LocDrawingInfo u (Point2 u) -> LocGraphic u
relativeTo mf upd = promote1 $ \pt -> 
    situ1 upd pt >>= \p2 -> mf `at` p2 


dot :: (Fractional u, FromPtSize u) => LocGraphic u
dot = dotNh `relativeTo` dotCenterline

period :: (Fractional u, FromPtSize u) => LocGraphic u
period = periodNh `relativeTo` periodCenterline

periodNh :: (Fractional u, FromPtSize u) => LocGraphic u
periodNh = scaleByCapHeight (1/12) >>= filledDisk

letter :: (Fractional u, FromPtSize u) => Char -> LocGraphic u
letter ch = textline [ch]


-- radius is 3/8 of cap height.
--
dotNh :: (Fractional u, FromPtSize u) => LocGraphic u
dotNh = scaleByCapHeight (3/8) >>= filledDisk


stemline :: (Fractional u, FromPtSize u) => LocGraphic u
stemline = scaleValue stem_length >>= (straightLine . vvec)

-- flam step is drawn from top 
--

flamPath :: FromPtSize u => DrawingInfo [Vec2 u]
flamPath = (\minor h -> [vec (-minor) (-minor), vvec (-h) ])
            <$> scaleValue flam_xminor <*> scaleValue flam_stem_length



flamstem :: (Fractional u, FromPtSize u) => LocGraphic u
flamstem = body `relativeTo` stemTop
  where  
    body = flamPath >>= openStrokePath
           




-- Note - line thickness should vary according to size...
--
oneStem :: (Fractional u, FromPtSize u) => LocGraphic u
oneStem = stemline `relativeTo` stemStart

-- Draws both flam and stem
--
oneFlam ::  (Fractional u, FromPtSize u) => LocGraphic u
oneFlam = body `relativeTo` stemStart
  where
    body = scaleValue stem_length  >>= \h  ->
           flamPath                >>= \vs ->
           openStrokePath (vvec h:vs)
           


evenStems :: (Fractional u, FromPtSize u) => Int -> LocGraphic u
evenStems n 
    | n <= 0    = error "evenStems - stem count must be 1 or more."
    | n == 1    = oneStem 
    | n == 2    = outer `relativeTo` stemStart 
    | otherwise = (outer `oplus` inner_stems) `relativeTo` stemStart
  where
    outer       = scaleValue stem_length >>= \h ->
                  scaleValue unit_width  >>= \w ->
                  upperRectPath (w * (fromIntegral $ n-1)) h
    inner_stems = scaleValue unit_width  >>= \w ->
                  multiDraw (n-2) (hvec w) stemline


multiDraw :: Num u => Int -> Vec2 u -> LocGraphic u -> LocGraphic u
multiDraw i _  _  | i <= 0 = error "multiDraw - empty"
multiDraw i v0 fn          = step (i-1) (v0 ^+^ v0) (shift v0)
  where
    shift v       = prepro1 (vecdisplace v) fn
    step 0 _ g    = g
    step n v g    = step (n-1) (v ^+^ v0) (g `oplus` shift v) 

           -- more to do



-- trace sw nw ne se - leave open at se.
-- 
upperRectPath :: Num u => u -> u -> LocGraphic u
upperRectPath w h = openStrokePath [ vvec h, hvec w, vvec (-h) ]

openStrokePath :: Num u => [Vec2 u] -> LocGraphic u
openStrokePath vs = promote1 $ \pt -> openStroke $ vectorPath pt vs