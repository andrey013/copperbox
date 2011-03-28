{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Text.Base.RotTextZero
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Direction zero (left-to-right) measured text that supports 
-- radial inclination. Caveat - rendering at any inclination other 
-- than the horizontal may not look good in PostScript or SVG.
--
-- \*\* WARNING \*\* - the API for this module needs some polish.
--
-- 
--------------------------------------------------------------------------------

module Wumpus.Drawing.Text.Base.RotTextZero
  ( 
    TextLine
  , textAlignLeft
  , textAlignCenter
  , textAlignRight

{-
    RotText
  , rotTextStart

  , textbox
  , rtextbox

  , multiAlignLeft
  , multiAlignCenter
  , multiAlignRight

  , textAlignLeft
  , textAlignRight
-}

  ) where

import Wumpus.Drawing.Text.Base.Common

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core


import Control.Applicative

type TextLine u = PosObject BoundingBox u


-- | Single line text with margins. 
-- 
-- Start point is baseline-left.
--
textAlignLeft :: (Real u, Floating u, InterpretUnit u) 
              => String -> LocImage BoundingBox u
textAlignLeft ss = 
    lift0R1 (makeTextLine ss) >>= \gf -> startPos gf BLL


-- | Single line text with mragins.
--
-- start point is baseline-center.
--
textAlignCenter :: (Real u, Floating u, InterpretUnit u) 
                => String -> LocImage BoundingBox u
textAlignCenter ss = 
    lift0R1 (makeTextLine ss) >>= \gf -> startPos gf BLC


-- | Single line text with margins. 
-- 
-- Start point is baseline-right.
--
textAlignRight :: (Real u, Floating u, InterpretUnit u) 
               => String -> LocImage BoundingBox u
textAlignRight ss = 
    lift0R1 (makeTextLine ss) >>= \gf -> startPos gf BLR 


makeTextLine :: InterpretUnit u => String -> Query (TextLine u)
makeTextLine ss = (\ortt -> makeBoundedPosObject ortt (escTextLine esc))
                    <$> textOrientationZero esc
  where
    esc = escapeString ss 



{-

type RotText u = PosThetaImage BoundingBox u


-------------------

-- | One line of multiline text
--
data OnelineText u = OnelineText 
        { text_content        :: EscapedText
        , oneline_adv         :: AdvanceVec u
        }



type OnelineGraphicF u = u -> OnelineText u -> LocThetaGraphic u




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
drawMultiline _     []  = lift1R3 emptyBoundedLocGraphic
drawMultiline drawF xs  = promoteR3 $ \start rpos ang ->
    linesToInterims xs                        >>= \(max_w, ones) ->
    borderedRotTextPos ang line_count max_w   >>= \opos -> 
    let gf         = multilineGraphic drawF max_w ang ones
        bbf        = orthoBB max_w line_count ang
        img        = intoLocImage bbf gf
        posG       = makePosImage opos img
    in atStartPos posG start rpos     
  where
    line_count = length xs


multilineGraphic :: (Floating u, InterpretUnit u)
                 => OnelineGraphicF u 
                 -> u 
                 -> Radian 
                 -> [OnelineText u]
                 -> LocGraphic u
multilineGraphic drawF max_w ang xs = 
    lift0R1 (centerSpineDisps (length xs) ang) >>= \(disp_top, disp_next) ->
    let gs         = map (\a -> rot (drawF max_w a) ang) xs
    in ignoreAns $ moveStart disp_top $ chainDisplace disp_next gs

-- | Draw left-aligned text. Effictively this is:
--
-- > Leftwards for the half the max vector
-- >
-- > Down to the baseline from the center.
--
onelineALeft :: (Real u, Floating u, InterpretUnit u)  
             => OnelineGraphicF u 
onelineALeft width otext = promoteR2 $ \ctr theta -> 
    centerToBaseline >>= \down -> 
    let pt = move down theta ctr 
    in atRot (rescTextLine $ text_content otext) pt theta
  where
    vec1      = hvec $ negate $ 0.5 * width
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
onelineACenter _ otext = promoteR2 $ \ctr theta -> 
    centerToBaseline >>= \down -> 
    let pt = move down theta ctr 
    in atRot (rescTextLine $ text_content otext) pt theta
  where
    vec1      = negateV $ 0.5 *^ oneline_adv otext
    move down = \ang -> thetaSouthwards down ang . displaceOrtho vec1 ang


-- | Draw right-aligned text. Effictively this is:
--
-- > Rightwards for the half the max width
-- >
-- > Leftwards for the width vector
-- >
-- > Down to the baseline from the center.
--
onelineARight :: (Real u, Floating u, InterpretUnit u)  
              => OnelineGraphicF u
onelineARight max_w otext = promoteR2 $ \ctr theta -> 
    centerToBaseline >>= \down -> 
    let pt = move down theta ctr 
    in atRot (rescTextLine $ text_content otext) pt theta
  where
    vec1      = hvec (0.5 * max_w) ^-^ oneline_adv otext
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
        => u -> Int -> Radian -> LocQuery u (BoundingBox u)
orthoBB w line_count theta = promoteR1 $ \ctr ->
    fmap (0.5*) verticalSpan          >>= \hh1 ->
    textMargin                        >>= \(xsep,ysep) -> 
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
-- largest width.
--
linesToInterims :: (InterpretUnit u, Ord u) 
                => [EscapedText] -> Query (u, [OnelineText u])
linesToInterims = fmap post . mapM onelineEscText
  where
    post xs                    = let vmax = foldr fn 0 xs in (vmax,xs)
    fn (OnelineText _ av) wmax = max (advanceH av) wmax




onelineEscText :: InterpretUnit u => EscapedText -> Query (OnelineText u)
onelineEscText esc = fmap (OnelineText esc) $ textVector esc


-}