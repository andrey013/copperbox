{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Djembe.Rhythm.GraphicInterpretation
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

module Wumpus.Rhythm.Djembe.GraphicInterpretation where

import Wumpus.Rhythm.Djembe.Base
import Wumpus.Rhythm.Djembe.GraphicPrimitives

import Wumpus.Basic.Kernel                      -- package: wumpus-basic
import Wumpus.Basic.System.FontLoader.Base

import Wumpus.Core                              -- package: wumpus-core



-- import Data.AffineSpace                         -- package: vector-space

import Control.Applicative
import Data.Ratio





-- | To generate output we need a Graphical interpretation.
--
-- newtype G = G { unG :: DLocGraphic }

-- type DNotehead = DLocImage NoteheadWidth



newtype G = G { getG :: DLocImage AfmUnit }


unG :: G -> DLocGraphic
unG = postpro1 snd . getG

mapG :: (DLocImage AfmUnit -> DLocImage AfmUnit) -> G -> G
mapG f a = G $ f $ getG a


instance CStrokeBase G where
  rest_note     = G $ restNote
  period        = G $ djembeNote $ periodNotehead

instance CStrokeAnno G where
  optional      = mapG addParens
  accent        = mapG (addAccent 584)
  lead_in       = mapG addLeadin
  dominant      = mapG addDominantHand
  other_hand    = mapG addOtherHand 



--------------------------------------------------------------------------------  

barLocGraphic :: Bar G -> DLocGraphic
barLocGraphic bar = extrLocGraphic $ barline `advplus` body `advplus` barline
  where
    body = barAdvGraphic bar

repLocGraphic :: Bar G -> DLocGraphic
repLocGraphic bar = extrLocGraphic $ lrepeat `advplus` body `advplus` rrepeat
  where
    body = barAdvGraphic bar


barAdvGraphic :: Bar G -> DAdvGraphic
barAdvGraphic b = superimposeAdvGraphic (barBeats b) (barBeamLines b)


barBeats :: Bar G -> DAdvGraphic
barBeats = advconcat . map groupBeats


groupBeats :: Group G -> DAdvGraphic
groupBeats = advconcat . map drawBeat


drawBeat :: Beat G -> DAdvGraphic
drawBeat (I  a)      = makeAdvGraphic advanceUnitWidth  (unG a)
drawBeat (S  a)      = makeAdvGraphic advanceUnitWidth  (drawSwing a)
drawBeat (Ha a b)    = makeAdvGraphic advanceUnitWidth  (drawHalved a b)
drawBeat (Pl n d xs) = makeAdvGraphic (advanceNUnits d) (drawPlets n d xs) 

drawSwing :: G -> DLocGraphic
drawSwing a = xminorMove (unG a) `oplus` swingStem

-- needs hline 
drawHalved :: G -> G -> DLocGraphic
drawHalved a b = unG a `oplus` halfBeam `oplus` halfUnitMove (unG b)


-- note plets missing top bracket...
--
drawPlets :: Int -> Int -> [G] -> DLocGraphic 
drawPlets n d xs = scaleValue (unit_width * realToFrac (d%n)) >>= \w -> 
                   explode (hvec w) $ map unG xs  
 

explode :: Num u => Vec2 u -> [LocGraphic u] -> LocGraphic u
explode v xs = extractLocGraphic $ advconcat $ map fn xs 
  where
    fn    = makeAdvGraphic (pure $ vecdisplace v)



xminorMove :: FromPtSize u => LocGraphic u -> LocGraphic u
xminorMove mg = scaleValue flam_xminor >>= \x -> prepro1 (hdisplace x) mg

halfUnitMove :: (Fractional u, FromPtSize u) => LocGraphic u -> LocGraphic u
halfUnitMove mg = scaleValue unit_width >>= \x -> prepro1 (hdisplace $ 0.5 * x) mg




type SpanWidth = Either UnitSpan (Int, Ratio Int)

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
    line (Right z)        = pletBeamLine z




advBeamLine :: Int -> DAdvGraphic 
advBeamLine i | i <= 0 = unitAdvGraphic
advBeamLine i          = let n = fromIntegral i in 
                         beamAdvGraphic (\uw -> uw * (n+1)) (\uw  -> uw * n)


shiftBeamLine :: Int -> DAdvGraphic 
shiftBeamLine i = 
    scaleValue flam_xminor >>= \minor    ->
    beamAdvGraphic (\uw -> uw * (n+1)) (\uw -> minor + uw * n)
  where
    n          = fromIntegral i


pletBeamLine :: (Int, Ratio Int) -> DAdvGraphic 
pletBeamLine (nrator,rw) = superimposeAdvGraphic beam_adv beam_bracket
  where
    beam_bracket  = pletBracket nrator rw
    beam_adv      = beamAdvGraphic (\uw -> uw * upper) (\uw -> uw * n)
    upper         = fromIntegral $ ceilingi rw
    n             = realToFrac rw
    
    ceilingi      :: Ratio Int -> Int
    ceilingi      = ceiling



beamAdvGraphic :: (Double -> Double) -> (Double -> Double) -> DAdvGraphic 
beamAdvGraphic advF drawF = 
    scaleValue unit_width >>= \uw -> makeAdvGraphic (adv uw) (obj uw)
  where
    adv = \uw -> pure $ hdisplace $ advF uw 
    obj = \uw -> localize capSquare $ 
                   scaleVMove stem_top (straightLine $ hvec $ drawF uw)


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
    outer (Pl n d _:zs)   = let pw = pletSpan n d in Right (n,pw) : outer zs
    
    inner w []            = cons w $ []
    inner w (I _:zs)      = inner (incrSpan w)  zs
    inner w (S _:zs)      = inner (shiftSpan w) zs
    inner w (Ha _ _:zs)   = inner (incrSpan w)  zs
    inner w (Pl n d _:zs) = let pw = pletSpan n d 
                            in cons w $ Right (n,pw) : outer zs
    
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
pletSpan n d = (d * (n-1)) % n


