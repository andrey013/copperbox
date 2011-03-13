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

  , textbox
  , rtextbox

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


type RotText u = PosThetaImage BoundingBox u


-------------------

-- | One line of multiline text
--
data OnelineText u = OnelineText 
        { text_content        :: EscapedText
        , oneline_adv         :: AdvanceVec u
        }



type OnelineGraphicF u = AdvanceVec u -> OnelineText u -> LocThetaGraphic u




rotTextStart :: PosThetaImage u a -> RectPosition -> Radian -> LocImage u a
rotTextStart = startPosRot


textbox :: (Real u, Floating u, InterpretUnit u) 
        => String -> PosImage BoundingBox u
textbox ss =  multiAlignCenter ss `ptRot` 0


rtextbox :: (Real u, Floating u, InterpretUnit u) 
         => String -> PosThetaImage BoundingBox u
rtextbox ss = multiAlignCenter ss


-- multi line text allows rotation 

multiAlignLeft :: (Real u, Floating u, InterpretUnit u) 
               => String -> PosThetaImage BoundingBox u
multiAlignLeft ss = 
   drawMultiline onelineALeft (map escapeString $ lines ss)

multiAlignCenter :: (Real u, Floating u, InterpretUnit u) 
               => String -> PosThetaImage BoundingBox u
multiAlignCenter ss = 
   drawMultiline onelineACenter (map escapeString $ lines ss)

multiAlignRight :: (Real u, Floating u, InterpretUnit u) 
               => String -> RotText u
multiAlignRight ss = 
   drawMultiline onelineARight (map escapeString $ lines ss)


textAlignLeft :: (Real u, Floating u, InterpretUnit u) 
              => String -> LocImage BoundingBox u
textAlignLeft ss = startPosRot (multiAlignLeft ss) CENTER 0

textAlignCenter :: (Real u, Floating u, InterpretUnit u) 
               => String -> LocImage BoundingBox u
textAlignCenter ss = startPosRot (multiAlignCenter ss) CENTER 0 

textAlignRight :: (Real u, Floating u, InterpretUnit u) 
               => String -> LocImage BoundingBox u
textAlignRight ss = startPosRot (multiAlignRight ss) CENTER 0





drawMultiline :: (Real u, Floating u, InterpretUnit u) 
              => OnelineGraphicF u -> [EscapedText] 
              -> PosThetaImage BoundingBox u
drawMultiline _     []  = lift_pti2 emptyBoundedLocGraphic
drawMultiline drawF xs  = promote_pti3 $ \start rpos theta ->
    bindQuery_i (linesToInterims xs) $ \(max_adv, ones) ->
    bindQuery_i (borderedRotTextPos theta line_count (advanceH max_adv)) $ \opos -> 
    bindQuery_i (centerSpineDisps line_count theta) $ \(disp_top, disp_next) ->
    let gs         = map (\a -> rot (drawF max_adv a) theta) ones
        gf         = moveStart disp_top $ chainDisplace disp_next gs
    in bindQuery_i (orthoBB max_adv line_count start theta) $ \ bb -> 
    let img        = intoLocImage (return $ \_ -> bb) (ignoreAns gf)
        posG       = posImage opos img
    in atStartPos posG start rpos     
  where
    line_count = length xs




-- | Draw left-aligned text. Effictively this is:
--
-- > Leftwards for the half the max vector
-- >
-- > Down to the baseline from the center.
--
onelineALeft :: (Real u, Floating u, InterpretUnit u)  
             => OnelineGraphicF u 
onelineALeft max_adv otext = promote_lti2 $ \ctr theta -> 
    bindQuery_i centerToBaseline $ \down -> 
      let pt = move down theta ctr 
      in atRot (rescapedline $ text_content otext) pt theta
  where
    vec1      = negateV $ 0.5 *^ max_adv
    move down = \ang -> thetaSouthwards down ang . displaceOrtho vec1 ang


-- | Draw center-aligned text. Effictively this is:
--
-- > Leftwards for the half the width vector
-- >
-- > Down to the baseline from the center.
--
-- The max_adv is ignored.
--
onelineACenter :: (Real u, Floating u, InterpretUnit u)  
               => OnelineGraphicF u
onelineACenter _ otext = promote_lti2 $ \ctr theta -> 
    bindQuery_i centerToBaseline $ \down -> 
      let pt = move down theta ctr 
      in atRot (rescapedline $ text_content otext) pt theta
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
onelineARight :: (Real u, Floating u, InterpretUnit u)  
              => OnelineGraphicF u
onelineARight max_adv otext = promote_lti2 $ \ctr theta -> 
    bindQuery_i centerToBaseline $ \down -> 
      let pt = move down theta ctr 
      in atRot (rescapedline $ text_content otext) pt theta
  where
    vec1      = (0.5 *^ max_adv) ^-^ oneline_adv otext
    move down = \ang -> thetaSouthwards down ang . displaceOrtho vec1 ang





-- Note - for multiline text, the bounding box (of one line) is 
-- always the same size regardless of the alignment of the textlines.
--

-- | Its easy to find top-left and top-right, then bottom-left is 
-- the vector from top-right to center added to the center. 
-- Likewise bottom-right is the vector from top-left-to center 
-- added to the center. Visually this construction forms a bow of 
-- two triangles meeting at the (rectangle) center.

orthoBB :: (Real u, Floating u, InterpretUnit u) 
        => AdvanceVec u -> Int -> Point2 u -> Radian -> Query (BoundingBox u)
orthoBB (V2 w _) line_count ctr theta = 
    fmap (0.5*) (info verticalSpan)     >>= \hh1 ->
    info textMargin                     >>= \(xsep,ysep) -> 
    centerSpineDisps line_count theta >>= \(disp_top,_) ->
    let top_ctr = disp_top ctr
        hw      = 0.5 * w
        tr      = displaceOrtho (V2 (hw+xsep) (hh1+ysep)) theta top_ctr
        tl      = displaceOrtho (V2 (negate $ hw+xsep) (hh1+ysep)) theta top_ctr
        bl      = ctr .+^ pvec tr ctr 
        br      = ctr .+^ pvec tl ctr
    in return $ traceBoundary [tr,tl,bl,br]



-- Note - displaceOrtho would be more convenient if it wasn\'t a 
-- vector.



--------------------------------------------------------------------------------

-- This isn't worth the complexity to get down to one traversal...

-- | Turn the input list of lines of 'EscapedText' into 
-- 'OnelineText' and return the result list twinned with the 
-- largest 'AdvanceVec'.
--
linesToInterims :: (InterpretUnit u, Ord u) 
                => [EscapedText] -> Query (AdvanceVec u, [OnelineText u])
linesToInterims = fmap post . mapM onelineEscText
  where
    post xs                    = let vmax = foldr fn (hvec 0) xs in (vmax,xs)
    fn (OnelineText _ av) vmax = avMaxWidth av vmax




avMaxWidth :: Ord u => AdvanceVec u -> AdvanceVec u -> AdvanceVec u
avMaxWidth a@(V2 w1 _) b@(V2 w2 _) = if w2 > w1 then b else a

onelineEscText :: InterpretUnit u => EscapedText -> Query (OnelineText u)
onelineEscText esc = fmap (OnelineText esc) $ textVector esc


