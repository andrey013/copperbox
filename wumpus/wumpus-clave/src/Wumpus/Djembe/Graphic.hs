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

import Wumpus.Djembe.Base

import Wumpus.Basic.Graphic                     -- package: wumpus-basic
import Wumpus.Basic.System.FontLoader.Base

import Wumpus.Core                              -- package: wumpus-core



import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace

import Control.Applicative
import Data.Ratio


-- empty_loc_graphic :: Num u => LocGraphic u
-- empty_loc_graphic = intoLocGraphic emptyPath 

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

period :: (Fractional u, FromPtSize u) => LocGraphic u
period = singleStemmed $ periodNh `relativeTo` vdispBasePt period_center

periodNh :: (Fractional u, FromPtSize u) => LocGraphic u
periodNh = scaleByCapHeight (1/12) >>= filledDisk

letter :: (Fractional u, FromPtSize u) => Char -> LocGraphic u
letter ch = singleStemmed $ textline [ch]

smallLetter :: (Fractional u, FromPtSize u) => Char -> LocGraphic u
smallLetter ch = getFontSize >>= \sz -> 
                 localize (fontSize $ (3 * sz) `div` 4) $ textline [ch]


-- radius is 3/8 of cap height.
--
dotNh :: (Fractional u, FromPtSize u) => LocGraphic u
dotNh = scaleByCapHeight (3/8) >>= filledDisk


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


evenStems :: (Fractional u, FromPtSize u) => Int -> LocGraphic u
evenStems n 
    | n <= 0    = error "evenStems - stem count must be 1 or more."
    | n == 1    = singleStem 
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
multiDraw i v0 fn          = step (i-1) (v0 ^+^ v0) (move v0)
  where
    move v        = prepro1 (vecdisplace v) fn
    step 0 _ g    = g
    step n v g    = step (n-1) (v ^+^ v0) (g `oplus` move v) 

           -- more to do



upperRectGraphic :: (Fractional u, FromPtSize u) => Ratio Int -> LocGraphic u
upperRectGraphic rw 
    | rw <= 0   = singleStem
    | otherwise = scaleValue stem_length >>= \h ->
                  scaleValue unit_width  >>= \uw ->
                  upperRectPath (uw * realToFrac rw) h


-- trace sw nw ne se - leave open at se.
-- 
upperRectPath :: Num u => u -> u -> LocGraphic u
upperRectPath w h = openStrokePath [ vvec h, hvec w, vvec (-h) ]

openStrokePath :: Num u => [Vec2 u] -> LocGraphic u
openStrokePath vs = promote1 $ \pt -> openStroke $ vectorPath pt vs



beamLine :: (Fractional u, FromPtSize u) => Ratio Int -> LocGraphic u
beamLine rw 
    | rw <= 0   = singleStem
    | otherwise = scaleValue unit_width  >>= \uw ->
                  straightLine (hvec $ uw * realToFrac rw) `relativeTo` stemTop



--------------------------------------------------------------------------------
-- 

-- | To generate output we need a Graphical interpretation.
--
newtype G = G { unG :: DLocGraphic }

instance CStroke G where
  optional nh = G $ unG nh  
  lead_in  nh = G $ unG nh
  accent   nh = G $ unG nh


barBeats :: Bar G -> DLocGraphic
barBeats = extractLocGraphic . step . map groupBeats
  where  
    step []     = makeAdvGraphic id emptyLocGraphic
    step (x:xs) = aconcat x xs


groupBeats :: Group G -> DAdvGraphic
groupBeats = step . map drawBeat
  where  
    step []     = makeAdvGraphic id emptyLocGraphic
    step (x:xs) = aconcat x xs


drawBeat :: Beat G -> DAdvGraphic
drawBeat (I  a)      = advanceUnitDisp >>= \fn -> makeAdvGraphic fn (unG a)
drawBeat (S  a)      = advanceUnitDisp >>= \fn -> 
                       makeAdvGraphic fn (xminorMove (unG a) `oplus` swingStem)
drawBeat (Ha a b)    = advanceUnitDisp >>= \fn -> 
                       makeAdvGraphic fn (unG a `oplus` halfUnitMove (unG b))
drawBeat (Pl n d xs) = advanceNDisp d >>= \fn -> 
                       makeAdvGraphic fn (drawPlets n d xs) 

-- note plets missing second stroke...
--
drawPlets :: Int -> Int -> [G] -> DLocGraphic 
drawPlets n d xs = scaleValue (unit_width * realToFrac (d%n)) >>= \w -> 
                   explode (hvec w) $ map unG xs  
 

explode :: Num u => Vec2 u -> [LocGraphic u] -> LocGraphic u
explode _ []     = emptyLocGraphic
explode v (x:xs) = extractLocGraphic $ aconcat (fn x) $ map fn xs 
  where
    fn    = makeAdvGraphic (vecdisplace v)

-- Note - plets are not joined to the rest of their group.

barBeamLines :: Bar G -> DLocGraphic
barBeamLines = extractLocGraphic . step . map groupBeamLines
  where
    step []     = makeAdvGraphic id emptyLocGraphic
    step (x:xs) = aconcat x xs


groupBeamLines :: Group G -> DAdvGraphic 
groupBeamLines = step . map advBeamLine . groupSpans
  where  
    step []     = makeAdvGraphic id emptyLocGraphic
    step (x:xs) = aconcat x xs

advanceUnitDisp :: FromPtSize u => DrawingInfo (PointDisplace u)
advanceUnitDisp = scaleValue unit_width >>= return . hdisplace


advanceNDisp :: FromPtSize u => Int-> DrawingInfo (PointDisplace u)
advanceNDisp i = 
    let n = fromIntegral i in 
    scaleValue unit_width >>= return . hdisplace . (*n)

xminorMove :: FromPtSize u => LocGraphic u -> LocGraphic u
xminorMove mg = scaleValue flam_xminor >>= \x -> prepro1 (hdisplace x) mg

halfUnitMove :: (Fractional u, FromPtSize u) => LocGraphic u -> LocGraphic u
halfUnitMove mg = scaleValue unit_width >>= \x -> prepro1 (hdisplace $ 0.5 * x) mg



advBeamLine :: (Int,Ratio Int) -> DAdvGraphic 
advBeamLine (swidth, drawing_width) = 
    scaleValue unit_width          >>= \uw    ->
    makeAdvGraphic (hdisplace $ uw * fromIntegral swidth) 
                   (beamLine drawing_width)


-- time_unit_width * real_drawing_width
--
-- Note - the count is one less than the elements
--
groupSpans :: Group G -> [(Int, Ratio Int)]
groupSpans xs = step 0 xs 
  where
    step w []            | w > 0     = [(ceiling w,w)]
                         | otherwise = []
    step w (I _ :zs)     = step (incr1 w) zs
    step w (S _ :zs)     = step (shift w) zs
    step w (Ha _ _:zs)   = step (incr1 w) zs
    step w (Pl n d _:zs) = let pw    = pletSpan n d 
                               consF = if w > 0 then ((ceiling w,w) :) else id
                           in consF $ (ceiling pw,pw) : step 0 zs

    

-- incr1 /cancels/ any extension due to a swing (shift).
--
incr1 :: Ratio Int -> Ratio Int
incr1 r = (1 + floor r) % 1

-- swing extends the line width by 1 (for the beat) plus a third 
-- for the swing shift.
--
-- Problem - adding a third is wrong... it should be the xminor instead
--
shift :: Ratio Int -> Ratio Int
shift r = (incr1 r) + 1%3


pletSpan :: Int -> Int -> Ratio Int
pletSpan n d = (d * n-1) % n



--------------------------------------------------------------------------------
-- These should be in Wumpus-Basic (possibly renamed)...

infixr 6 `aplus`

aplus :: AdvGraphic u -> AdvGraphic u -> AdvGraphic u
aplus = accumulate1 oplus

aconcat :: AdvGraphic u -> [AdvGraphic u] -> AdvGraphic u
aconcat a []     = a
aconcat a (x:xs) = aconcat (a `aplus` x) xs 

extractLocGraphic :: AdvGraphic u -> LocGraphic u
extractLocGraphic = postpro1 snd
