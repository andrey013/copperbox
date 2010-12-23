{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Text.LRText
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Left-to-right measured text. The text uses glyph metrics so it 
-- can be positioned accurately.
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Text.LRText
  ( 

    baseCenterLine
  , baseLeftLine
  , baseRightLine

  , rbaseCenterLine
  , rbaseLeftLine
  , rbaseRightLine

  , ctrCenterLine

  , multiAlignLeft
  , multiAlignCenter
  , multiAlignRight

  , rmultiAlignLeft
  , rmultiAlignCenter
  , rmultiAlignRight
 

  ) where


import Wumpus.Basic.Kernel

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Core.Text.GlyphIndices

import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace

import Control.Applicative
import Data.Char
import qualified Data.Map               as Map
import Data.Maybe 


-- Note - BoundedLocThetaGraphic is probably an adequate type
-- even though the same text will have a different bounding box
-- if it is rotated (the sides of the BBox are always parallel to 
-- the x and y axes even if the text is not parrale to the 
-- x-axis). 
-- 
-- I cannot think of any compelling graphics that need a more 
-- accurate type. The execption is text cannot have exact anchors 
-- however this is a moot /if/ text is considered as a labelling 
-- of an existing rectangle (which may or may not have been 
-- rotated).
--



-- One line of multiline text
--
data OnelineText u = OnelineText EscapedText (AdvanceVec u)


-- | max_width * oneline_text -> LocThetaGraphic
--
type LocThetaDrawOneline u = u -> OnelineText u -> LocThetaGraphic u


-- | max_width * oneline_text -> LocThetaGraphic
--
type BoundedLocThetaOneline u = u -> OnelineText u -> BoundedLocThetaGraphic u


-- | Draw one line of left-aligned text, knowing the max_width 
-- of all the lines of text.
--
-- All left-aligned text is moved left by half the max_width.
--
-- Note - implicit point is baseline-center, this is perhaps 
-- unintituitive given the functions name but it is an 
-- advantage for drawing multi-line text.
-- 
drawLeftAligned :: Floating u => LocThetaDrawOneline u
drawLeftAligned max_width (OnelineText esc _) = 
    promoteR2 $ \baseline_ctr theta -> 
       let mv = displaceParallel ((-0.5) * max_width) theta
       in apply2R2 (rescapedline esc) (mv baseline_ctr) theta
       


-- | Draw one line of center-aligned text. Center aligned text is 
-- oblivious to the max_width of all the lines of text.
--
-- Each line of center-aligned text is moved left by half its 
-- advance vector.
--
-- Implicit point is baseline-center.
--
drawCenterAligned :: Floating u => LocThetaDrawOneline u
drawCenterAligned _ (OnelineText esc av) = 
    promoteR2 $ \baseline_ctr theta -> 
       let mv = displaceParallel (negate $ 0.5 * advanceH av) theta
       in apply2R2 (rescapedline esc) (mv baseline_ctr) theta
       

-- | Draw one line of right-aligned text, knowing the max_width 
-- of all the lines of text.
--
-- Each right-aligned text line is moved by the width component 
-- of the advance vector minus half the max width.
--
-- Note - implicit point is baseline-center, this is perhaps 
-- unintituitive given the functions name but it is an 
-- advantage for drawing multi-line text.
-- 
drawRightAligned :: Floating u => LocThetaDrawOneline u
drawRightAligned max_width (OnelineText esc av) = 
    promoteR2 $ \baseline_ctr theta -> 
      let mv = displaceParallel ((0.5 * max_width) - advanceH av) theta
      in apply2R2 (rescapedline esc) (mv baseline_ctr) theta




-- Impilict point is baseline-center.
--
onelineBBox :: (Real u, Floating u, FromPtSize u) 
            => OnelineText u -> LocThetaDrawingInfo u (BoundingBox u)
onelineBBox (OnelineText _ av) = 
    promoteR2 $ \baseline_ctr theta -> 
      glyphHeightRange >>= \(ymin, ymax) ->
        let hw = 0.5 * advanceH av 
            bl = baseline_ctr .+^ vec (-hw) ymin
            tr = baseline_ctr .+^ vec hw    ymax
        in pure $ centerOrthoBBox theta (BBox bl tr)





-- This should have max_width as a param...
--
makeMoveableLine :: (Real u, Floating u, FromPtSize u) 
                 => LocThetaDrawOneline u 
                 -> BoundedLocThetaOneline u
makeMoveableLine drawF max_width oline =
      intoLocThetaImage (onelineBBox oline) (drawF max_width oline)

onelineAlg :: (Real u, Floating u, FromPtSize u) 
           => DisplaceFun u 
           -> LocThetaDrawOneline u 
           -> String 
           -> BoundedLocThetaGraphic u
onelineAlg ptMoveF drawF ss = 
   promoteR2 $ \pt theta -> 
     onelineText ss >>= \ans@(OnelineText _ av) ->
       let max_width = advanceH av
           move      = ptMoveF max_width ans theta 
       in apply2R2 (makeMoveableLine drawF max_width ans) (move pt) theta





-- | Draw 1 line...
--
-- Impilict point is baseline-left.
--

baseLeftLine :: (Real u, Floating u, FromPtSize u) 
              => String -> BoundedLocGraphic u
baseLeftLine ss = rbaseLeftLine ss `rot` 0



-- | Draw 1 line...
--
-- Impilict point is baseline-center.
--
baseCenterLine :: (Real u, Floating u, FromPtSize u) 
               => String -> BoundedLocGraphic u
baseCenterLine ss = rbaseCenterLine ss `rot` 0




-- | Draw 1 line...
--
-- Impilict point is baseline-right.
--
baseRightLine :: (Real u, Floating u, FromPtSize u) 
              => String -> BoundedLocGraphic u
baseRightLine ss = rbaseRightLine ss `rot` 0



rbaseLeftLine :: (Real u, Floating u, FromPtSize u) 
              => String -> BoundedLocThetaGraphic u
rbaseLeftLine = onelineAlg leftToCenter drawLeftAligned


rbaseCenterLine :: (Real u, Floating u, FromPtSize u) 
                => String -> BoundedLocThetaGraphic u
rbaseCenterLine = onelineAlg centerToCenter drawCenterAligned


rbaseRightLine :: (Real u, Floating u, FromPtSize u) 
              => String -> BoundedLocThetaGraphic u
rbaseRightLine = onelineAlg rightToCenter drawRightAligned



ctrCenterLine :: (Real u, Floating u, FromPtSize u) 
              => String -> BoundedLocGraphic u
ctrCenterLine ss = baseCenterLine ss



-- | max_width * interim_text * theta -> (Point -> Point)
--
type DisplaceFun u = u -> OnelineText u -> Radian -> PointDisplace u

centerToCenter :: DisplaceFun u
centerToCenter _ _ _ = id

leftToCenter :: Floating u => DisplaceFun u
leftToCenter max_width _ theta =
    displaceParallel (0.5 * max_width) theta

rightToCenter :: Floating u => DisplaceFun u
rightToCenter max_width (OnelineText _ av) theta =
    displaceParallel ((0.5 * max_width) - advanceH av) theta




multiAlignLeft :: (Floating u, Real u, Ord u, FromPtSize u)
               => String
               -> BoundedLocGraphic u
multiAlignLeft ss = rmultiAlignLeft ss `rot` 0


multiAlignCenter :: (Floating u, Real u, Ord u, FromPtSize u)
                 => String
                 -> BoundedLocGraphic u
multiAlignCenter ss = rmultiAlignCenter ss `rot` 0




multiAlignRight :: (Floating u, Real u, Ord u, FromPtSize u)
                => String
                -> BoundedLocGraphic u
multiAlignRight ss = rmultiAlignRight ss `rot` 0


rmultiAlignLeft :: (Floating u, Real u, Ord u, FromPtSize u)
               => String
               -> BoundedLocThetaGraphic u
rmultiAlignLeft = multilineTEXT (makeMoveableLine drawLeftAligned)


rmultiAlignCenter :: (Floating u, Real u, Ord u, FromPtSize u)
                  => String
                  -> BoundedLocThetaGraphic u
rmultiAlignCenter = multilineTEXT (makeMoveableLine drawCenterAligned)


rmultiAlignRight :: (Floating u, Real u, Ord u, FromPtSize u)
                 => String
                 -> BoundedLocThetaGraphic u
rmultiAlignRight = multilineTEXT (makeMoveableLine drawRightAligned)



multilineTEXT :: (Floating u, Ord u, FromPtSize u)
              => BoundedLocThetaOneline u
              -> String
              -> BoundedLocThetaGraphic u
multilineTEXT _  [] = lift1R2 emptyBoundedLocGraphic
multilineTEXT mf ss = 
    lift0R2 (linesToInterims ss) >>= \(max_av, itexts) -> 
      centralPoints (length itexts) >>= \pts -> 
        zipMultis (advanceH max_av) mf itexts pts


      

zipMultis :: (Ord u, FromPtSize u)
          => u
          -> BoundedLocThetaOneline u
          -> [OnelineText u] -> [Point2 u]
          -> BoundedLocThetaGraphic u
zipMultis _     _  []     _       = lift1R2 $ emptyBoundedLocGraphic
zipMultis _     _  _      []      = lift1R2 $ emptyBoundedLocGraphic
zipMultis max_w mf (a:as) (b:bs) = step a b as bs
  where
    mkGraphic itext pt        = promoteR2 $ \_ theta -> 
                                  apply2R2 (mf max_w itext) pt theta
    step r s (r2:rs) (s2:ss)  = liftA2 oplus (mkGraphic r s) (step r2 s2 rs ss)
    step r s _       _        = mkGraphic r s




-- | @ana@ is an /anacrusis/ factor - if there are even points
-- half the baseline_spacing is added to get the top point
--
centralPoints :: Floating u => Int -> LocThetaDrawingInfo u [Point2 u]
centralPoints n | n < 2     = promoteR2 $ \ctr _  -> return [ctr]
                | even n    = body (n `div` 2) (0.5*)
                | otherwise = body (n `div` 2) (0 *) 
  where
    body halfn ana  = promoteR2 $ \ctr theta -> 
                        baselineSpacing >>= \h ->
                          let y0  = (h * fromIntegral halfn) + ana h
                              top = displacePerpendicular y0 theta ctr
                          in pure $ trailPoints n h theta top
        


trailPoints :: Floating u => Int -> u -> Radian -> Point2 u -> [Point2 u]
trailPoints n height theta top = take n $ iterate fn top
  where
    fn pt = displacePerpendicular (-height) theta pt



--------------------------------------------------------------------------------

-- This isn't worth the complexity to get down to one traversal...

linesToInterims :: (FromPtSize u, Ord u) 
                => String -> DrawingInfo (AdvanceVec u, [OnelineText u])
linesToInterims = fmap post . mapM onelineText . lines
  where
    post xs                    = let vmax = foldr fn (hvec 0) xs in (vmax,xs)
    fn (OnelineText _ av) vmax = avMaxWidth av vmax

avMaxWidth :: Ord u => AdvanceVec u -> AdvanceVec u -> AdvanceVec u
avMaxWidth a@(V2 w1 _) b@(V2 w2 _) = if w2 > w1 then b else a

onelineText :: FromPtSize u => String -> DrawingInfo (OnelineText u)
onelineText ss = 
    let esc = escapeString ss in fmap (OnelineText esc) $ textVector esc


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
