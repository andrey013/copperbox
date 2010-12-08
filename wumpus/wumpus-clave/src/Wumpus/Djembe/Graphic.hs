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

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Basic.System.FontLoader.Base

import Wumpus.Core                              -- package: wumpus-core



import Data.AffineSpace                         -- package: vector-space

import Control.Applicative
import Data.Ratio



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

fullstop :: (Fractional u, FromPtSize u) => LocGraphic u
fullstop = singleStemmed $ periodNh `relativeTo` vdispBasePt period_center

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


openStrokePath :: Num u => [Vec2 u] -> LocGraphic u
openStrokePath vs = promote1 $ \pt -> openStroke $ vectorPath pt vs


--------------------------------------------------------------------------------
-- 


barLocGraphic :: Bar G -> DLocGraphic
barLocGraphic b = barBeats b `oplus` barBeamLines b

-- | To generate output we need a Graphical interpretation.
--
newtype G = G { unG :: DLocGraphic }

instance CStroke G where
  optional nh = G $ unG nh  
  lead_in  nh = G $ unG nh
  accent   nh = G $ unG nh


barBeats :: Bar G -> DLocGraphic
barBeats = extractLocGraphic . advconcat . map groupBeats


groupBeats :: Group G -> DAdvGraphic
groupBeats = advconcat . map drawBeat


drawBeat :: Beat G -> DAdvGraphic
drawBeat (I  a)      = makeAdvGraphic advanceUnitDisp  (unG a)
drawBeat (S  a)      = makeAdvGraphic advanceUnitDisp  (drawSwing a)
drawBeat (Ha a b)    = makeAdvGraphic advanceUnitDisp  (drawHalved a b)
drawBeat (Pl n d xs) = makeAdvGraphic (advanceNDisp d) (drawPlets n d xs) 

drawSwing :: G -> DLocGraphic
drawSwing a = xminorMove (unG a) `oplus` swingStem

-- needs hline 
drawHalved :: G -> G -> DLocGraphic
drawHalved a b = unG a `oplus` halfUnitMove (unG b)


-- note plets missing top bracket...
--
drawPlets :: Int -> Int -> [G] -> DLocGraphic 
drawPlets n d xs = scaleValue (unit_width * realToFrac (d%n)) >>= \w -> 
                   explode (hvec w) $ map unG xs  
 

explode :: Num u => Vec2 u -> [LocGraphic u] -> LocGraphic u
explode v xs = extractLocGraphic $ advconcat $ map fn xs 
  where
    fn    = makeAdvGraphic (pure $ vecdisplace v)


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




type SpanWidth = Either UnitSpan (Ratio Int)

data UnitSpan = Whole Int | Shift Int
  deriving (Eq,Show)


barBeamLines :: Bar G -> DLocGraphic
barBeamLines = extractLocGraphic . advconcat . map groupBeamLines
 
groupBeamLines :: Group G -> DAdvGraphic
groupBeamLines = step . groupSpans
  where
    step []               = unitAdvGraphic
    step [x]              = line x
    step (x:xs)           = line x `advplus` step xs
    line (Left (Whole n)) = advBeamLine n
    line (Left (Shift n)) = shiftBeamLine n
    line (Right r)        = pletBeamLine r


beamAdvGraphic :: (Double -> Double) -> (Double -> Double) -> DAdvGraphic 
beamAdvGraphic f g = 
    scaleValue unit_width >>= \uw -> makeAdvGraphic (adv uw) (obj uw)
  where
    adv = \uw -> pure $ hdisplace $ f uw 
    obj = \uw -> localize capSquare $ 
                   straightLine (hvec $ g uw) `relativeTo` stemTop



advBeamLine :: Int -> DAdvGraphic 
advBeamLine i | i <= 1 = unitAdvGraphic         -- check this...
advBeamLine i          = 
    let n = fromIntegral i in 
    beamAdvGraphic (\uw -> uw * (n+1)) (\uw  -> uw * n)


shiftBeamLine :: Int -> DAdvGraphic 
shiftBeamLine i = 
    scaleValue flam_xminor >>= \minor    ->
    beamAdvGraphic (\uw -> uw * (n+1)) (\uw -> minor + uw * n)
  where
    n          = fromIntegral i


pletBeamLine :: Ratio Int -> DAdvGraphic 
pletBeamLine i = 
    beamAdvGraphic (\uw -> uw * upper) (\uw -> uw * n)
  where
    upper      = fromIntegral $ ceilingi i
    n          = realToFrac i
    
    ceilingi   :: Ratio Int -> Int
    ceilingi   = floor

unitAdvGraphic :: DAdvGraphic
unitAdvGraphic = scaleValue unit_width >>= \uw ->
                 makeAdvGraphic (pure $ vecdisplace $ hvec uw) emptyLocGraphic



-- Note - counting behaviour is rather convoluted (the first beat 
-- is not counted).
--
-- 1 beat  - has a count 0
-- 2 beats - count 1
-- 3 beats - count 2
--
-- But N-plets are special - stop the current span, calculate the 
-- plet span separately, carry on.
--
-- Shifts are special too - shift only extend the width if the 
-- last beat is a shift. Shifts within a beam group simply 
-- borrow from the next note.
--
groupSpans :: Group G -> [SpanWidth]
groupSpans xs = outer xs 
  where
    outer []              = []     
    outer (I _:zs)        = inner zeroSpan zs
    outer (S _:zs)        = inner (Shift 0) zs
    outer (Ha _ _:zs)     = inner zeroSpan zs
    outer (Pl n d _:zs)   = let pw = pletSpan n d in Right pw : outer zs
    
    inner w []            = cons w $ []
    inner w (I _:zs)      = inner (incrSpan w)  zs
    inner w (S _:zs)      = inner (shiftSpan w) zs
    inner w (Ha _ _:zs)   = inner (incrSpan w)  zs
    inner w (Pl n d _:zs) = let pw = pletSpan n d 
                            in cons w $ Right pw : outer zs
    
    cons (Whole n) | n <= 0 = id
    cons w                  = \ls -> Left w : ls

zeroSpan :: UnitSpan
zeroSpan = Whole 0

incrSpan :: UnitSpan -> UnitSpan
incrSpan (Whole n) = Whole (n+1)
incrSpan (Shift n) = Whole (n+1)        -- back to whole

shiftSpan :: UnitSpan -> UnitSpan
shiftSpan (Whole n) = Shift (n+1)
shiftSpan (Shift n) = Shift (n+1)       -- this should be unnecessary


pletSpan :: Int -> Int -> Ratio Int
pletSpan n d = (d * n-1) % n


