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

    RotText

  , rotTextStart

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


type RotText u = PosThetaImage u (BoundingBox u)


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
    AdvanceVec u -> OnelineText u -> LocThetaImage u (BoundingBox u)



rotTextStart :: PosThetaImage u a -> RectPosition -> Radian -> LocImage u a
rotTextStart = apply2R3


singleLine :: (Real u, Floating u, FromPtSize u) 
           => String -> PosImage u (BoundingBox u)
singleLine ss = apply1R3 (onelineDraw onelineACenter (escapeString ss)) 0


escSingleLine :: (Real u, Floating u, FromPtSize u) 
              => EscapedText -> PosImage u (BoundingBox u)
escSingleLine esc = apply1R3 (onelineDraw onelineACenter esc) 0


rsingleLine :: (Real u, Floating u, FromPtSize u) 
            => String -> PosThetaImage u (BoundingBox u)
rsingleLine ss = onelineDraw onelineACenter (escapeString ss)


rescSingleLine :: (Real u, Floating u, FromPtSize u) 
               => EscapedText -> PosThetaImage u (BoundingBox u)
rescSingleLine = onelineDraw onelineACenter


-- multi line text allows rotation 

multiAlignLeft :: (Real u, Floating u, FromPtSize u) 
               => String -> PosThetaImage u (BoundingBox u)
multiAlignLeft ss = 
   drawMultiline onelineALeft (map escapeString $ lines ss)

multiAlignCenter :: (Real u, Floating u, FromPtSize u) 
               => String -> PosThetaImage u (BoundingBox u)
multiAlignCenter ss = 
   drawMultiline onelineACenter (map escapeString $ lines ss)

multiAlignRight :: (Real u, Floating u, FromPtSize u) 
               => String -> PosThetaImage u (BoundingBox u)
multiAlignRight ss = 
   drawMultiline onelineARight (map escapeString $ lines ss)


textAlignLeft :: (Real u, Floating u, FromPtSize u) 
              => String -> LocImage u (BoundingBox u)
textAlignLeft ss = apply2R3 (multiAlignLeft ss) CENTER 0

textAlignCenter :: (Real u, Floating u, FromPtSize u) 
               => String -> LocImage u (BoundingBox u)
textAlignCenter ss = apply2R3 (multiAlignCenter ss) CENTER 0 

textAlignRight :: (Real u, Floating u, FromPtSize u) 
               => String -> LocImage u (BoundingBox u)
textAlignRight ss = apply2R3 (multiAlignRight ss) CENTER 0




-- Note - inclination is not part of the ContextFunction...
-- Maybe it should be but then we need an CF3 (arity 3)

drawMultiline :: (Real u, Floating u, FromPtSize u) 
              => OnelineDrawF u -> [EscapedText] 
              -> PosThetaImage u (BoundingBox u)
drawMultiline _     []  = lift1R3 emptyBoundedLocGraphic
drawMultiline drawF [x] = onelineDraw drawF x
drawMultiline drawF xs  = promoteR3 $ \start rpos theta ->
    linesToInterims xs >>= \(max_adv, ones) -> 
    borderedRotTextPos theta line_count (advanceH max_adv) >>= \opos -> 
    let chn   = centerSpinePoints line_count theta
        gs    = map (\a -> apply1R2 (drawF max_adv a) theta) ones
        gf    = unchainZip emptyBoundedLocGraphic gs chn
        posG  = makePosImage opos gf
    in  atStartPos posG start rpos     
  where
    line_count = length xs




onelineDraw :: (Real u, Floating u, FromPtSize u) 
            => OnelineDrawF u -> EscapedText -> PosThetaImage u (BoundingBox u)
onelineDraw drawF esc = promoteR3 $ \start rpos theta ->
    onelineEscText esc        >>= \otext -> 
    borderedRotTextPos theta 1 (advanceH $ oneline_adv otext) >>= \opos  -> 
    let max_adv = oneline_adv otext 
        gf      = apply1R2 (drawF max_adv otext) theta
        posG    = makePosImage opos gf
    in atStartPos posG start rpos 



-- | Draw left-aligned text. Effictively this is:
--
-- > Leftwards for the half the max vector
-- >
-- > Down to the baseline from the center.
--
onelineALeft :: (Real u, Floating u, FromPtSize u)  
             => OnelineDrawF u 
onelineALeft max_adv otext = promoteR2 $ \ctr theta -> 
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
onelineACenter max_adv otext = promoteR2 $ \ctr theta -> 
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
onelineARight max_adv otext = promoteR2 $ \ctr theta -> 
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
    getTextMargin     >>= \(xsep,ysep) -> 
    let hw  = 0.5 * w
        hh  = 0.5 * h
        bl  = ctr .-^ V2 (hw + xsep) (hh + ysep)
        mv  = V2 (w + xsep + xsep) (h + ysep + ysep)
        bb1 = boundingBox bl (bl .+^ mv)
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


