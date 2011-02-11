{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Text.RotTextLR
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

module Wumpus.Drawing.Text.RotTextLR
  ( 

    RotTextLR

  , singleLine
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
import Wumpus.Drawing.Text.Base

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace


type RotTextLR u = PosImage u (BoundingBox u)


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




-- Note - inclination is not part of the ContextFunction...
-- Maybe it should be but then we need an CF3 (arity 3)

drawMultiline :: (Real u, Floating u, FromPtSize u) 
              => OnelineDrawF u -> Radian -> [EscapedText] 
              -> PosImage u (BoundingBox u)
drawMultiline _     _     []  = lift1R2 emptyBoundedLocGraphic
drawMultiline drawF theta [x] = onelineDraw drawF theta x
drawMultiline drawF theta xs  = promoteR2 $ \start rpos ->
    linesToInterims xs >>= \(max_adv, ones) -> 
    borderedRotTextPos theta line_count (advanceH max_adv) >>= \opos -> 
    let chn   = centerSpinePoints line_count theta
        gs    = map (drawF theta max_adv) ones
        gf    = unchainZip emptyBoundedLocGraphic gs chn
        posG  = makePosImage opos gf
    in  atStartPos posG start rpos     
  where
    line_count = length xs




onelineDraw :: (Real u, Floating u, FromPtSize u) 
            => OnelineDrawF u -> Radian -> EscapedText -> PosImage u (BoundingBox u)
onelineDraw drawF theta esc = promoteR2 $ \start rpos ->
    onelineEscText esc        >>= \otext -> 
    borderedRotTextPos theta 1 (advanceH $ oneline_adv otext) >>= \opos  -> 
    let max_adv = oneline_adv otext 
        gf      = drawF theta max_adv otext
        posG    = makePosImage opos gf
    in  atStartPos posG start rpos 







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


