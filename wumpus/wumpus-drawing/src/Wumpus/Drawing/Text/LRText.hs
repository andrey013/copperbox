{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Text.LRText
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Left-to-right measured text. The text uses glyph metrics so it 
-- can be positioned accurately.
-- 
-- Note multi-line text does not have an intuitive notion of 
-- baseline. It should be positioned with @CENTER@ or the compass
-- @RectPosition@s.
-- 
-- 
-- \*\* WARNING \*\* - the API for this module needs work. The 
-- current API is not satisfactory for drawing according to a 
-- start position (there are other reasonable start positions than 
-- the ones currently supported - adding them would explode the 
-- number of definitions).
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Text.LRText
  ( 

    singleLine
  , escSingleLine

  , multiAlignLeft
  , multiAlignCenter
  , multiAlignRight

  , textAlignCenter
  , textAlignLeft
  , textAlignRight

  ) where


import Wumpus.Basic.Kernel

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Core.Text.GlyphIndices

import Data.VectorSpace                         -- package: vector-space

import Control.Applicative
import Control.Monad
import Data.Char
import qualified Data.Map               as Map
import Data.Maybe 


-- Note - BoundedLocThetaGraphic is probably an adequate type
-- even though the same text will have a different bounding box
-- if it is rotated (the sides of the BBox are always parallel to 
-- the x and y axes even if the text is not parrale to the 
-- x-axis). 
-- 
-- I\'m not convinced of any compelling graphics that need a more 
-- accurate type - @text@ can be considered as a labelling of an 
-- existing Shape, rather than a shape-like thing itself.
--


-- One line of multiline text
--
data OnelineText u = OnelineText 
        { text_content        :: EscapedText
        , oneline_width       :: AdvanceVec u
        }



makeSingleLine :: (Real u, Floating u, FromPtSize u) 
               => EscapedText 
               -> DrawingInfo (PosImage u (BoundingBox u))
makeSingleLine esc = 
    onelineEscText esc >>= \otext -> makeOneline leftOPos otext

-- Single line text can always be made with @leftOPos@ as there
-- is no notion of justification for single lines.

singleLine :: (Real u, Floating u, FromPtSize u) 
           => RectPosition -> String -> BoundedLocThetaGraphic u
singleLine rpos ss = promoteR2 $ \pt theta -> 
    makeSingleLine (escapeString ss) >>= \graphic ->
    atRot (setPosition rpos graphic) pt theta


escSingleLine :: (Real u, Floating u, FromPtSize u) 
              => RectPosition -> EscapedText -> BoundedLocThetaGraphic u
escSingleLine rpos esc = promoteR2 $ \pt theta -> 
    makeSingleLine esc >>= \graphic ->
    atRot (setPosition rpos graphic) pt theta




type OPosF u = u -> (u,u) -> (u,u) -> ObjectPos u 


-- | This is baseline-left with mx added to both horizontal directions 
-- and my added to both vertical directions.
--
leftOPos :: Num u => OPosF u 
leftOPos width (ch,dd) (mx,my) = 
    ObjectPos { op_x_minor = mx
              , op_x_major = width + mx 
              , op_y_minor = (negate dd) + my  
              , op_y_major = ch + my
              }

-- Note - dd usually (should be...) negative but ObjectPos minors 
-- should be positive. 


makeOneline :: (Real u, Floating u, FromPtSize u) 
             => OPosF u -> OnelineText u 
             -> DrawingInfo (PosImage u (BoundingBox u))
makeOneline oposF otext = 
    glyphCapHeight   >>= \ch ->
    glyphDescender   >>= \dd ->                 -- note - usually negative
    getTextMargin    >>= \(mx, my) -> 
    let width = advanceH $ oneline_width otext 
        opos  = oposF  width (ch,dd) (mx,my)
        bbF   = onelineBBox width (ch,dd) (mx,my)
        gF    = rescapedline $ text_content otext
    in return $ makePosImage opos (intoLocThetaImage bbF gF)



-- | This is baseline-left with mx added to both horizontal directions 
-- and my added to both vertical directions.
-- 
-- descender_depth expected to be negative...
--
onelineBBox :: (Real u, Floating u) 
             => u -> (u,u) -> (u,u) -> LocThetaCF u (BoundingBox u)
onelineBBox adv_width (ch,dd) (mx,my) = 
    promoteR2 $ \p0@(P2 x y) theta -> 
      let ll   = P2 (x - mx)             (y - (my + (negate dd)))
          ur   = P2 (x + adv_width + mx) (y + ch + my) 
          bbox = boundingBox ll ur
      in return $ rotBBox p0 theta bbox 


rotBBox :: (Real u, Floating u) 
        => Point2 u -> Radian -> BoundingBox u -> BoundingBox u
rotBBox pt theta bb = 
    traceBoundary $ map (rotateAbout theta pt) ps
  where
    ps  = boundaryCornerList bb

--------------------------------------------------------------------------------

multiAlignLeft :: (Real u, Floating u, Ord u, FromPtSize u) 
               => RectPosition -> String -> BoundedLocThetaGraphic u
multiAlignLeft rpos ss = promoteR2 $ \pt theta -> 
    makeMultiPosGraphic NW nwMultiPos ss >>= \graphic ->
    atRot (setPosition rpos graphic) pt theta

multiAlignCenter :: (Real u, Floating u, Ord u, FromPtSize u) 
                 => RectPosition -> String -> BoundedLocThetaGraphic u
multiAlignCenter rpos ss = promoteR2 $ \pt theta -> 
    makeMultiPosGraphic NN nnMultiPos ss >>= \graphic ->
    atRot (setPosition rpos graphic) pt theta


multiAlignRight :: (Real u, Floating u, Ord u, FromPtSize u) 
                => RectPosition -> String -> BoundedLocThetaGraphic u
multiAlignRight rpos ss = promoteR2 $ \pt theta -> 
    makeMultiPosGraphic NE neMultiPos ss >>= \graphic ->
    atRot (setPosition rpos graphic) pt theta


-- | Simpler version of 'multiAlignLeft' - always positioned at
-- center, always horizontal (not rotated).
--
textAlignLeft :: (Real u, Floating u, Ord u, FromPtSize u) 
                => String -> BoundedLocGraphic u
textAlignLeft ss = multiAlignLeft CENTER ss `rot` 0


-- | Simpler version of 'multiAlignCenter' - always positioned at
-- center, always horizontal (not rotated).
--
textAlignCenter :: (Real u, Floating u, Ord u, FromPtSize u) 
                => String -> BoundedLocGraphic u
textAlignCenter ss = multiAlignCenter CENTER ss `rot` 0



-- | Simpler version of 'multiAlignLeft' - always positioned at
-- center, always horizontal (not rotated).
--
textAlignRight :: (Real u, Floating u, Ord u, FromPtSize u) 
                => String -> BoundedLocGraphic u
textAlignRight ss = multiAlignRight CENTER ss `rot` 0




type MultiPosBuilder u = u -> Int -> DrawingInfo (ObjectPos u)


makeMultiPosGraphic :: (Real u, Floating u, Ord u, FromPtSize u) 
                    => RectPosition -> MultiPosBuilder u -> String 
                    -> DrawingInfo (PosImage u (BoundingBox u))
makeMultiPosGraphic rpos buildF str = 
    linesToInterims str >>= \(av_max,ss) -> 
    let width = advanceH av_max
    in buildF width (length ss) >>= \opos   ->
       return $ makePosImage opos (multiLocTheta rpos ss)




nwMultiPos :: (Fractional u, FromPtSize u) => MultiPosBuilder u 
nwMultiPos width line_count = 
    multilineTextHeight line_count >>= \height   ->
    getTextMargin                  >>= \(mx, my) -> 
    return $ ObjectPos { op_x_minor = mx
                       , op_x_major = width + mx 
                       , op_y_minor = height + my  
                       , op_y_major = my
                       }


nnMultiPos :: (Fractional u, FromPtSize u) => MultiPosBuilder u
nnMultiPos width line_count = 
    multilineTextHeight line_count >>= \height   ->
    getTextMargin                  >>= \(mx, my) -> 
    let center_x = 0.5 * (mx + width + mx) 
    in return $ ObjectPos { op_x_minor = center_x
                          , op_x_major = center_x
                          , op_y_minor = height + my  
                          , op_y_major = my
                          }



neMultiPos :: (Fractional u, FromPtSize u) => MultiPosBuilder u
neMultiPos width line_count = 
    multilineTextHeight line_count >>= \height   ->
    getTextMargin                  >>= \(mx, my) -> 
    return $ ObjectPos { op_x_minor = mx + width
                       , op_x_major = mx 
                       , op_y_minor = height + my
                       , op_y_major = my
                       }




multiLocTheta :: (Real u, Floating u, FromPtSize u)
              => RectPosition -> [OnelineText u] -> BoundedLocThetaGraphic u
multiLocTheta _    [] = emptyLocThetaGraphic
multiLocTheta rpos xs = promoteR2 $ \pt theta -> 
    trailPointsDown theta pt >>= \ps ->
    let gs = zipWith (mf theta) xs ps 
    in safeconcat (emptyBoundedLocGraphic `at` pt) gs
  where
    mf theta ln pt = atRot (escSingleLine rpos (text_content ln)) pt theta



-- for Wumpus-Basic?
emptyLocThetaGraphic :: Num u => BoundedLocThetaGraphic u
emptyLocThetaGraphic = lift1R2 emptyBoundedLocGraphic


multilineTextHeight :: (Fractional u, FromPtSize u) => Int -> DrawingInfo u
multilineTextHeight n | n >  1    = liftM2 (+) oneHeight (spacings $ n-1)
                      | n == 1    = oneHeight
                      | otherwise = return 0
  where
    oneHeight  = liftM2 (\h d -> h-d) glyphCapHeight glyphDescender
    spacings i = baselineSpacing >>= \h -> return (h * fromIntegral i)
    




-- | 'trailMoves' : @ y_dist * theta * point -> [Point] @
--
-- Build a chain of points downwards from the top.
--
-- Builds an infinite list...
--
trailPointsDown :: Floating u 
                => Radian -> Point2 u -> DrawingInfo [Point2 u]
trailPointsDown theta top = 
    baselineSpacing >>= \h -> pure $ iterate (fn h) top 
  where
    fn h pt = displacePerpendicular (-h) theta pt

           


--------------------------------------------------------------------------------

-- This isn't worth the complexity to get down to one traversal...

linesToInterims :: (FromPtSize u, Ord u) 
                => String -> DrawingInfo (AdvanceVec u, [OnelineText u])
linesToInterims = fmap post . mapM (onelineEscText . escapeString) . lines
  where
    post xs                    = let vmax = foldr fn (hvec 0) xs in (vmax,xs)
    fn (OnelineText _ av) vmax = avMaxWidth av vmax

avMaxWidth :: Ord u => AdvanceVec u -> AdvanceVec u -> AdvanceVec u
avMaxWidth a@(V2 w1 _) b@(V2 w2 _) = if w2 > w1 then b else a

onelineEscText :: FromPtSize u => EscapedText -> DrawingInfo (OnelineText u)
onelineEscText esc = fmap (OnelineText esc) $ textVector esc



textVector :: FromPtSize u => EscapedText -> DrawingInfo (AdvanceVec u)
textVector esc = 
    cwLookupTable >>= \table -> 
       let cs = destrEscapedText id esc 
       in pure $ foldr (\c v -> v ^+^ (charWidth table c)) (vec 0 0) cs
     
   


charWidth :: FromPtSize u 
          => CharWidthTable u -> EscapedChar -> AdvanceVec u
charWidth fn (CharLiteral c) = fn $ ord c
charWidth fn (CharEscInt i)  = fn i
charWidth fn (CharEscName s) = fn ix
  where
    ix = fromMaybe (-1) $ Map.lookup s ps_glyph_indices
