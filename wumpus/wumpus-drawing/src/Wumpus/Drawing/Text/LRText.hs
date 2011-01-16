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

  , multiLineLeft
  , multiLineCenter
  , multiLineRight

  , baseCenterLine
  , baseLeftLine
  , baseRightLine

  , rbaseCenterLine
  , rbaseLeftLine
  , rbaseRightLine

  , ctrCenterLine
  , baseCenterEscChar

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
-- I cannot think of any compelling graphics that need a more 
-- accurate type. The execption is text cannot have exact anchors 
-- however this is a moot /if/ text is considered as a labelling 
-- of an existing rectangle (which may or may not have been 
-- rotated).
--


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



-- One line of multiline text
--
data OnelineText u = OnelineText 
        { text_content        :: EscapedText
        , oneline_width       :: AdvanceVec u
        }


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
        bbF   = onelineBBox' width (ch,dd) (mx,my)
        gF    = rescapedline $ text_content otext
    in return $ makePosImage opos (intoLocThetaImage bbF gF)



-- | This is baseline-left with mx added to both horizontal directions 
-- and my added to both vertical directions.
-- 
-- descender_depth expected to be negative...
--
onelineBBox' :: (Real u, Floating u) 
             => u -> (u,u) -> (u,u) -> LocThetaCF u (BoundingBox u)
onelineBBox' adv_width (ch,dd) (mx,my) = 
    promoteR2 $ \(P2 x y) theta -> 
      let ll   = P2 (x - mx)             (y - (my + (negate dd)))
          ur   = P2 (x + adv_width + mx) (y + ch + my) 
          bbox = boundingBox ll ur
      in return $ centerOrthoBBox theta bbox 


--------------------------------------------------------------------------------

multiLineLeft :: (Real u, Floating u, Ord u, FromPtSize u) 
              => RectPosition -> String -> BoundedLocThetaGraphic u
multiLineLeft rpos ss = promoteR2 $ \pt theta -> 
    makeMultiPosGraphic NW nwMultiPos ss >>= \graphic ->
    atRot (setPosition rpos graphic) pt theta

multiLineCenter :: (Real u, Floating u, Ord u, FromPtSize u) 
                => RectPosition -> String -> BoundedLocThetaGraphic u
multiLineCenter rpos ss = promoteR2 $ \pt theta -> 
    makeMultiPosGraphic NN nnMultiPos ss >>= \graphic ->
    atRot (setPosition rpos graphic) pt theta


multiLineRight :: (Real u, Floating u, Ord u, FromPtSize u) 
               => RectPosition -> String -> BoundedLocThetaGraphic u
multiLineRight rpos ss = promoteR2 $ \pt theta -> 
    makeMultiPosGraphic NE neMultiPos ss >>= \graphic ->
    atRot (setPosition rpos graphic) pt theta




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
    in safeconcat' (emptyBoundedLocGraphic `at` pt) gs
  where
    mf theta ln pt = atRot (escSingleLine rpos (text_content ln)) pt theta

-- Wumpus-Basic needs changing...
--
safeconcat' :: OPlus a => Image u a -> [Image u a] -> Image u a    
safeconcat' _   (x:xs) = oconcat x xs
safeconcat' alt []     = alt


-- for Wumpus-Basic
emptyLocThetaGraphic :: Num u => BoundedLocThetaGraphic u
emptyLocThetaGraphic = lift1R2 emptyBoundedLocGraphic


-- Not right...

multilineTextHeight :: (Fractional u, FromPtSize u) => Int -> DrawingInfo u
multilineTextHeight n | n >  1    = liftM2 (+) oneHeight (spacings $ n-1)
                      | n == 1    = oneHeight
                      | otherwise = return 0
  where
    oneHeight  = liftM2 (\h d -> h-d) glyphCapHeight glyphDescender
    spacings i = baselineSpacing >>= \h -> return (h * fromIntegral i)
    



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
      getTextMargin    >>= \(xsep, ysep) -> 
        let hw        = 0.5 * advanceH av 
            btm_left  = baseline_ctr .+^ vec (-hw) ymin
            top_right = baseline_ctr .+^ vec hw    ymax
            bbox      = expandBB xsep ysep (BBox btm_left top_right)
        in pure $ centerOrthoBBox theta bbox
  where
    expandBB xsep ysep (BBox (P2 x0 y0) (P2 x1 y1)) = 
        BBox (P2 (x0-xsep) (y0-ysep)) (P2 (x1+xsep) (y1+ysep))
    






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
           -> EscapedText
           -> BoundedLocThetaGraphic u
onelineAlg ptMoveF drawF esc = 
   promoteR2 $ \pt theta -> 
     onelineEscText esc >>= \ans@(OnelineText _ av) ->
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
rbaseLeftLine ss = 
    onelineAlg leftToCenter drawLeftAligned (escapeString ss)


rbaseCenterLine :: (Real u, Floating u, FromPtSize u) 
                => String -> BoundedLocThetaGraphic u
rbaseCenterLine ss = 
    onelineAlg centerToCenter drawCenterAligned (escapeString ss)


rbaseRightLine :: (Real u, Floating u, FromPtSize u) 
              => String -> BoundedLocThetaGraphic u
rbaseRightLine ss = 
    onelineAlg rightToCenter drawRightAligned (escapeString ss)


-- Note - assumes the ymin of the font is 0 or less.
--
ctrCenterLine :: (Real u, Floating u, FromPtSize u) 
              => String -> BoundedLocGraphic u
ctrCenterLine ss =
    glyphHeightRange >>= \(ymin, ymax) -> 
      let hh = 0.5 * ymax - ymin in 
        moveStartPoint (displaceV $ negate $ hh - abs ymin) $ baseCenterLine ss



baseCenterEscChar :: (Real u, Floating u, FromPtSize u) 
                  => EscapedChar -> BoundedLocGraphic u
baseCenterEscChar esc = body `rot` 0
  where
    body = onelineAlg centerToCenter drawCenterAligned (wrapEscChar esc)






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
        

-- | 'trailPoints' : @ count * y_dist * theta * point -> [Point] @
--
-- Build a chain of points downwards from the top.
--
trailPoints :: Floating u => Int -> u -> Radian -> Point2 u -> [Point2 u]
trailPoints n height theta top = take n $ iterate fn top
  where
    fn pt = displacePerpendicular (-height) theta pt



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
