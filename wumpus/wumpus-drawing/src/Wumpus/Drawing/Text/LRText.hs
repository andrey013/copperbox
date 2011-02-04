{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Text.LRText
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Left-to-right measured text that supports radial inclination.
-- Caveat - rendering at any degree other than the horizontal may
-- not look good in PostScript or SVG.
-- 
-- \*\* WARNING \*\* - the API for this module needs some polish.
--
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Text.LRText
  ( 

    singleLine
  , escSingleLine
  , rsingleLine
  , rescSingleLine

  , multiAlignLeft
  , multiAlignCenter
  , multiAlignRight

  , textAlignCenter
  , textAlignLeft
  , textAlignRight

  ) where

import Wumpus.Drawing.Chains

import Wumpus.Basic.Kernel

import Wumpus.Core                              -- package: wumpus-core
import Wumpus.Core.Text.GlyphIndices

import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace

import Control.Applicative
import Data.Char
import qualified Data.Map               as Map
import Data.Maybe 

--
-- Note - margins are not added to the text. This seems to be the 
-- right thing to do in the case of rotated text, where the ortho
-- projection of the rectangle already can add spacing between 
-- the RectPos and the actual text.
--


-------------------

-- | One line of multiline text
--
data OnelineText u = OnelineText 
        { text_content        :: EscapedText
        , oneline_adv         :: AdvanceVec u
        }

-- Design note - using a LocThetaImage could be used instead, but
-- as the angle of inclination is interior to the final type the 
-- it is used explicitly.
--
type OnelineDrawF u = 
    Radian -> AdvanceVec u -> OnelineText u -> LocImage u (BoundingBox u)




singleLine :: (Real u, Floating u, FromPtSize u) 
           => String -> PosImage u (BoundingBox u)
singleLine ss = onelineDraw onelineACenter 0 (escapeString ss)

escSingleLine :: (Real u, Floating u, FromPtSize u) 
              => EscapedText -> PosImage u (BoundingBox u)
escSingleLine = onelineDraw onelineACenter 0


rsingleLine :: (Real u, Floating u, FromPtSize u) 
            => Radian -> String -> PosImage u (BoundingBox u)
rsingleLine theta ss = onelineDraw onelineACenter theta (escapeString ss)

rescSingleLine :: (Real u, Floating u, FromPtSize u) 
               => Radian -> EscapedText -> PosImage u (BoundingBox u)
rescSingleLine = onelineDraw onelineACenter

multiAlignLeft :: (Real u, Floating u, FromPtSize u) 
               => Radian -> String -> PosImage u (BoundingBox u)
multiAlignLeft theta ss = 
   drawMultiline onelineALeft theta (map escapeString $ lines ss)

multiAlignCenter :: (Real u, Floating u, FromPtSize u) 
               => Radian -> String -> PosImage u (BoundingBox u)
multiAlignCenter theta ss = 
   drawMultiline onelineACenter theta (map escapeString $ lines ss)

multiAlignRight :: (Real u, Floating u, FromPtSize u) 
               => Radian -> String -> PosImage u (BoundingBox u)
multiAlignRight theta ss = 
   drawMultiline onelineARight theta (map escapeString $ lines ss)


textAlignLeft :: (Real u, Floating u, FromPtSize u) 
              => String -> LocImage u (BoundingBox u)
textAlignLeft ss = multiAlignLeft 0 ss `startPos` CENTER

textAlignCenter :: (Real u, Floating u, FromPtSize u) 
               => String -> LocImage u (BoundingBox u)
textAlignCenter ss = multiAlignCenter 0 ss `startPos` CENTER 

textAlignRight :: (Real u, Floating u, FromPtSize u) 
               => String -> LocImage u (BoundingBox u)
textAlignRight ss = multiAlignRight 0 ss `startPos` CENTER 



emptyLine :: Num u => LocImage u (BoundingBox u)
emptyLine = promoteR1 $ \start -> 
               replaceAns (BBox start start) $ emptyLocGraphic `at` start


-- Note - inclination is not part of the ContextFunction...

drawMultiline :: (Real u, Floating u, FromPtSize u) 
              => OnelineDrawF u -> Radian -> [EscapedText] 
              -> PosImage u (BoundingBox u)
drawMultiline _     _     []  = lift1R2 emptyLine 
drawMultiline drawF theta [x] = onelineDraw drawF theta x
drawMultiline drawF theta xs  = promoteR2 $ \start rpos ->
    linesToInterims xs >>= \(max_adv, ones) -> 
    multilineObjectPos line_count theta max_adv >>= \opos -> 
    centerSpinePoints  line_count theta >>= \pts -> 
    let gs    = map (drawF theta max_adv) ones
        gf    = zipchainM emptyLine gs pts
        posG  = makePosImage opos gf
    in  atStartPos posG start rpos     
  where
    line_count = length xs



-- | Note this is not a zip if it has an alt... 
--
zipchainM :: OPlus a 
          => LocImage u a -> [LocImage u a] -> LocChain u -> LocImage u a
zipchainM alt []          _  = promoteR1 $ \pt -> alt `at` pt 
zipchainM alt (img1:imgs) fn = promoteR1 $ \pt -> case fn pt of
    []      -> alt `at` pt
    (p1:ps) -> go (img1 `at` p1) imgs ps
  where
    go acc (g:gs) (p:ps)   = let ans = (g `at` p) in go (acc `oplus` ans) gs ps
    go acc _      _        = acc




onelineDraw :: (Real u, Floating u, FromPtSize u) 
            => OnelineDrawF u -> Radian -> EscapedText -> PosImage u (BoundingBox u)
onelineDraw drawF theta esc = promoteR2 $ \start rpos ->
    onelineEscText esc        >>= \otext -> 
    singlelineObjectPos theta (oneline_adv otext) >>= \opos  -> 
    let max_adv = oneline_adv otext 
        gf      = drawF theta max_adv otext
        posG    = makePosImage opos gf
    in  atStartPos posG start rpos 


-- | Height of multiline text is cap_height to descender for the 
-- first line, then baseline-to-baseline span for the remaining
-- lines.
--
multilineObjectPos :: (Real u, Floating u, FromPtSize u) 
                   => Int -> Radian -> AdvanceVec u -> DrawingInfo (ObjectPos u)
multilineObjectPos line_count theta max_adv =
    glyphVerticalSpan >>= \h1 ->
    baselineSpacing   >>= \bspan ->
    let hw    = 0.5 * (advanceH max_adv)
        spans = bspan * fromIntegral (line_count - 1)
        hh    = 0.5 * (h1 + spans)
    in return $ orthoObjectPos theta $ ObjectPos hw hw hh hh 




-- | Center-center ObjectPos of OnelineText drawn with no 
-- inclination.
-- 
--
singlelineObjectPos :: (Real u, Floating u, FromPtSize u) 
                    => Radian -> AdvanceVec u -> DrawingInfo (ObjectPos u)
singlelineObjectPos theta max_adv = 
    fmap (0.5*) glyphVerticalSpan >>= \hh ->
    let hw = 0.5 * (advanceH max_adv)
    in return $ orthoObjectPos theta $ ObjectPos hw hw hh hh 
    

-- Note - this returns the answer in center form, regardless
-- of whether the input was in center form.
--
orthoObjectPos :: (Real u, Floating u) 
               => Radian -> ObjectPos u -> ObjectPos u
orthoObjectPos theta (ObjectPos xmin xmaj ymin ymaj) = 
    ObjectPos bbox_hw bbox_hw bbox_hh bbox_hh
  where
    input_hw  = 0.5 * (xmin + xmaj)
    input_hh  = 0.5 * (ymin + ymaj)
    bbox0     = BBox (P2 (-input_hw) (-input_hh)) (P2 input_hw input_hh)
    bbox1     = retraceBoundary (rotateAbout theta zeroPt) bbox0
    bbox_hw   = 0.5 * (boundaryWidth bbox1)
    bbox_hh   = 0.5 * (boundaryHeight bbox1)







-- | Draw left-aligned text. Effictively this is:
--
-- > Leftwards for the half the max vector
-- >
-- > Down to the baseline from the center.
--
onelineALeft :: (Real u, Floating u, FromPtSize u)  
             => OnelineDrawF u 
onelineALeft theta max_adv otext = promoteR1 $ \ctr -> 
    centerToBaseline >>= \down -> 
    atRot (orthoBB max_adv) ctr theta >>= \bbox -> 
    let pt = move down theta ctr 
    in replaceAns bbox $ atRot (rescapedline $ text_content otext) pt theta
  where
    vec1      = negateV $ 0.5 *^ max_adv
    move down = \ang -> thetaSouthwards down ang . displaceOrtho vec1 ang


-- | Draw center-aligned text. Effictively this is:
--
-- > Leftwards for the half the width vector
-- >
-- > Down to the baseline from the center.
--
onelineACenter :: (Real u, Floating u, FromPtSize u)  
               => OnelineDrawF u
onelineACenter theta max_adv otext = promoteR1 $ \ctr -> 
    centerToBaseline >>= \down -> 
    atRot (orthoBB max_adv) ctr theta >>= \bbox ->  
    let pt = move down theta ctr 
    in replaceAns bbox $ atRot (rescapedline $ text_content otext) pt theta
  where
    vec1      = negateV $ 0.5 *^ oneline_adv otext
    move down = \ang -> thetaSouthwards down ang . displaceOrtho vec1 ang


-- | Draw right-aligned text. Effictively this is:
--
-- > Rightwards for the half the max vector
-- >
-- > Leftwards for the width vector
-- >
-- > Down to the baseline from the center.
--
onelineARight :: (Real u, Floating u, FromPtSize u)  
              => OnelineDrawF u
onelineARight theta max_adv otext = promoteR1 $ \ctr -> 
    centerToBaseline >>= \down -> 
    atRot (orthoBB max_adv) ctr theta >>= \bbox -> 
    let pt = move down theta ctr 
    in replaceAns bbox $ atRot (rescapedline $ text_content otext) pt theta
  where
    vec1      = (0.5 *^ max_adv) ^-^ oneline_adv otext
    move down = \ang -> thetaSouthwards down ang . displaceOrtho vec1 ang



-- Note - for multiline text, the bounding box (of one line) is 
-- always the same size regardless of the alignment of the textlines.
--
orthoBB :: (Real u, Floating u, FromPtSize u) 
        => AdvanceVec u -> LocThetaDrawingInfo u (BoundingBox u)
orthoBB (V2 w _) = promoteR2 $ \ctr theta -> 
    glyphVerticalSpan >>= \h ->
    let bl  = ctr .-^ V2 (0.5 * w) (0.5 * h)
        bb1 = boundingBox bl (bl .+^ V2 w h)
        bb2 = retraceBoundary (rotateAbout theta ctr) bb1
    in return bb2


-- | Calculate the distance from the center of a one-line textbox 
-- to the baseline. Note the height of a textbox is @vspan@ which 
-- is cap_height + descender
--
centerToBaseline :: (Fractional u, FromPtSize u) => DrawingInfo u
centerToBaseline = 
    (\ch vspan -> ch - 0.5 * vspan) <$> glyphCapHeight <*> glyphVerticalSpan

-- | All drawing is done on a spine that plots points from the 
-- first line of text. The spine is calculated from its center and
-- has to account for the inclination.
--
centerSpinePoints :: Floating u 
                  => Int -> Radian -> DrawingInfo (LocChain u)
centerSpinePoints n theta
    | n <= 1    = return (\pt -> [pt])
    | otherwise = baselineSpacing >>= \h1 -> 
                  let dist_top = h1 * centerCount n
                      mktop    = \ctr -> thetaNorthwards dist_top theta ctr    
                  in return (\ctr -> take n $ iterate (thetaSouthwards h1 theta) 
                                                      (mktop ctr))




-- | Count the steps from the center to an end:
--
-- >
-- > 1 = 0     .
-- > 2 = 0.5   .__.
-- > 3 = 1     .__.__.
-- > 4 = 1.5   .__.__.__.
-- >
--
centerCount :: Fractional u => Int -> u
centerCount i = (fromIntegral i) / 2 - 0.5


--------------------------------------------------------------------------------

-- This isn't worth the complexity to get down to one traversal...

-- | Turn the input list of lines of 'EscapedText' into 
-- 'OnelineText' and return the result list twinned with the 
-- largest 'AdvanceVec'.
--
linesToInterims :: (FromPtSize u, Ord u) 
                => [EscapedText] -> DrawingInfo (AdvanceVec u, [OnelineText u])
linesToInterims = fmap post . mapM onelineEscText
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

