{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Arrows.Tips
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Anchor points on shapes.
--
-- \*\* WARNING \*\* this module is an experiment, and may 
-- change significantly in future revisions.
-- 
--------------------------------------------------------------------------------

module Wumpus.Basic.Arrows.Tips
  ( 


    Arrowhead(..)
 
  , tri90
  , tri60
  , tri45
  , otri90
  , otri60
  , otri45

  , revtri90
  , revtri60
  , revtri45
  , orevtri90
  , orevtri60
  , orevtri45

  , barb90
  , barb60
  , barb45
  , revbarb90
  , revbarb60
  , revbarb45

  , perp

  , bracket

  , diskTip
  , odiskTip
  , squareTip
  , osquareTip
  , diamondTip
  , odiamondTip

  ) where

import Wumpus.Basic.Graphic
import Wumpus.Basic.Utils.Combinators

import Wumpus.Core                      -- package: wumpus-core

import Data.AffineSpace                 -- package: vector-space

import Control.Applicative


-- | Encode an arrowhead as an image where the /answer/ is the
-- retract distance.
--
-- The retract distance is context sensitive - usually just on
-- the markHeight (or halfMarkHeight) so it has to be calculated
-- w.r.t. the DrawingCtx.
--
newtype Arrowhead u = Arrowhead { getArrowhead :: ThetaLocImage u u }




-- | Tiplen is length of the tip \*along the line it follows\*. 
--
-- > |\
-- > | \
-- > | /
-- > |/
-- > 
-- > |  |  -- tip len
--

-- | Tip width is the distance between upper and lower 
-- arrow points.
--
-- >       __
-- > |\    
-- > | \   tip
-- > | /   width
-- > |/    __
-- >       



-- | This one is for triangular tips defined by their tip angle
-- e.g. 90deg, 60deg, 45deg.
--
-- The tip width will be variable (tip length should be the 
-- markHeight).
-- 
triVecsByAngle :: Floating u => u -> Radian -> Radian -> (Vec2 u, Vec2 u)
triVecsByAngle tiplen halfang theta = (vec_to_upper, vec_to_lower)
  where
    hypo_len     = tiplen / (fromRadian $ cos halfang)
    rtheta       = pi + theta        -- theta in the opposite direction
    vec_to_upper = avec (circularModulo $ rtheta - halfang) hypo_len
    vec_to_lower = avec (circularModulo $ rtheta + halfang) hypo_len 


{-
-- | This one is for triangles when the tip height and tip width
-- are known.
--
triVecsByDist  :: (Real u, Floating u) 
               => u -> u -> Radian -> (Vec2 u, Vec2 u)
triVecsByDist tiplen half_tipwidth theta = (vec_to_upper, vec_to_lower)
  where
    hypo_len     = sqrt $ (tiplen*tiplen) + (half_tipwidth*half_tipwidth)
    halfang      = toRadian $ atan (half_tipwidth / tiplen) 
    rtheta       = pi + theta        -- theta in the opposite direction
    vec_to_upper = avec (circularModulo $ rtheta - halfang) hypo_len
    vec_to_lower = avec (circularModulo $ rtheta + halfang) hypo_len 
-}



{-
markHeightPlusLineWidth :: (Fractional u, FromPtSize u) => DrawingR u
markHeightPlusLineWidth = 
    (\h lw -> h + realToFrac lw) <$> markHeight <*> lineWidth
-}


markHeightLessLineWidth :: (Fractional u, FromPtSize u) => DrawingR u
markHeightLessLineWidth = 
    (\h lw -> h - realToFrac lw) <$> markHeight <*> lineWidth


-- noRetract ignores both the angle and the point.
--
-- Its common for the rectraction not to care about the angle or 
-- the point and only care about the DrawingCtx.
--
noRetract :: Num u => ThetaLocDrawingR u u
noRetract = rlift2 $ pure 0 


--------------------------------------------------------------------------------

-- | Tripoints takes the \*tip length\* is the mark height.
--
-- This means that the 90deg tip has a tip width greater-than the
-- mark height (but that is okay - seemingly this is how TikZ 
-- does it).
--
tripointsByAngle :: (Floating u, FromPtSize u)
                 => Radian ->  ThetaLocDrawingR u (Point2 u, Point2 u)
tripointsByAngle triang theta tip = 
    (\h -> let (vupper,vlower) = triVecsByAngle h (0.5*triang) theta
           in  (tip .+^ vupper, tip .+^ vlower))
      <$> markHeight


revtripointsByAngle :: (Floating u, FromPtSize u)
                    => Radian 
                    ->  ThetaLocDrawingR u (Point2 u, Point2 u, Point2 u)
revtripointsByAngle triang theta tip = 
    (\h -> let theta'          = circularModulo $ pi+theta 
               (vupper,vlower) = triVecsByAngle h (0.5*triang) theta'
               back_tip        = tip .-^ avec theta h 
           in (back_tip .+^ vupper, back_tip, back_tip .+^ vlower) )
      <$> markHeight




{-
tripointsByDist :: (Real u, Floating u, FromPtSize u)
                => (u -> u) -> (u -> u)  
                -> ThetaLocDrawingR u (Point2 u, Point2 u)
tripointsByDist lenF halfwidthF theta tip = 
    (\h -> let (vup,vlo) = triVecsByDist (lenF h) (halfwidthF $ 0.5*h) theta
           in  (tip .+^ vup, tip .+^ vlo))
      <$> markHeight
-}



-- width = xchar_height
-- filled with stroke colour!

triTLG :: (Floating u, Real u, FromPtSize u)
       => Radian -> (PrimPath u -> Graphic u) -> ThetaLocGraphic u
triTLG triang drawF = tripointsByAngle triang `bindR2` \(u,v) -> 
    rlift1 $ \pt -> localize bothStrokeColour $ drawF $ vertexPath [pt,u,v]





tri90 :: (Floating u, Real u, FromPtSize u) => Arrowhead u
tri90 = Arrowhead $ 
    intoThetaLocImage (rlift2 markHeight) (triTLG (pi/2) filledPath)


tri60 :: (Floating u, Real u, FromPtSize u) => Arrowhead u
tri60 = Arrowhead $
    intoThetaLocImage (rlift2 markHeight) (triTLG (pi/3) filledPath)


tri45 :: (Floating u, Real u, FromPtSize u) => Arrowhead u
tri45 = Arrowhead $ 
    intoThetaLocImage (rlift2 markHeight) (triTLG (pi/4) filledPath)

otri90 :: (Floating u, Real u, FromPtSize u) => Arrowhead u
otri90 = Arrowhead $
    intoThetaLocImage (rlift2 markHeight) (triTLG (pi/2) closedStroke)

otri60 :: (Floating u, Real u, FromPtSize u) => Arrowhead u
otri60 = Arrowhead $   
    intoThetaLocImage (rlift2 markHeight) (triTLG (pi/3) closedStroke)

otri45 :: (Floating u, Real u, FromPtSize u) => Arrowhead u
otri45 = Arrowhead $ 
    intoThetaLocImage (rlift2 markHeight) (triTLG (pi/4) closedStroke)



-- width = xchar_height
-- filled with stroke colour!

revtriTLG :: (Floating u, Real u, FromPtSize u)
          => Radian -> (PrimPath u -> Graphic u) -> ThetaLocGraphic u
revtriTLG triang drawF = revtripointsByAngle triang `bindR2` \(u,pt,v) -> 
    rlift2 $ localize bothStrokeColour $ drawF $ vertexPath [u,pt,v]


revtri90 :: (Floating u, Real u, FromPtSize u) => Arrowhead u
revtri90 = Arrowhead $ 
    intoThetaLocImage (rlift2 markHeightLessLineWidth) 
                      (revtriTLG (pi/2) filledPath)

revtri60 :: (Floating u, Real u, FromPtSize u) => Arrowhead u
revtri60 = Arrowhead $ 
    intoThetaLocImage (rlift2 markHeightLessLineWidth) 
                      (revtriTLG (pi/3) filledPath)

revtri45 :: (Floating u, Real u, FromPtSize u) => Arrowhead u
revtri45 = Arrowhead $ 
    intoThetaLocImage (rlift2 markHeightLessLineWidth) 
                      (revtriTLG (pi/4) filledPath)


orevtri90 :: (Floating u, Real u, FromPtSize u) => Arrowhead u
orevtri90 = Arrowhead $ 
    intoThetaLocImage (rlift2 markHeightLessLineWidth) 
                      (revtriTLG (pi/2) closedStroke)

orevtri60 :: (Floating u, Real u, FromPtSize u) => Arrowhead u
orevtri60 = Arrowhead $ 
    intoThetaLocImage (rlift2 markHeightLessLineWidth) 
                      (revtriTLG (pi/3) closedStroke)

orevtri45 :: (Floating u, Real u, FromPtSize u) => Arrowhead u
orevtri45 = Arrowhead $ 
    intoThetaLocImage (rlift2 markHeightLessLineWidth) 
                      (revtriTLG (pi/4) closedStroke)


barbTLG :: (Floating u, Real u, FromPtSize u) => Radian -> ThetaLocGraphic u
barbTLG ang = tripointsByAngle ang `bindR2` \(u,v) -> 
    rlift1 $ \pt -> openStroke $ vertexPath [u,pt,v]


barb90 :: (Floating u, Real u, FromPtSize u) => Arrowhead u
barb90 = Arrowhead $ intoThetaLocImage noRetract (barbTLG (pi/2))

barb60 :: (Floating u, Real u, FromPtSize u) => Arrowhead u
barb60 = Arrowhead $ intoThetaLocImage noRetract (barbTLG (pi/3))


barb45 :: (Floating u, Real u, FromPtSize u) => Arrowhead u
barb45 = Arrowhead $ intoThetaLocImage noRetract (barbTLG (pi/4))



revbarbTLG :: (Floating u, Real u, FromPtSize u) => Radian -> ThetaLocGraphic u
revbarbTLG ang = revtripointsByAngle ang `bindR2` \(u,pt,v) -> 
    rlift2 $ openStroke $ vertexPath [u,pt,v]

revbarb90 :: (Floating u, Real u, FromPtSize u) => Arrowhead u
revbarb90 = Arrowhead $ 
    intoThetaLocImage (rlift2 markHeight) (revbarbTLG (pi/2))


revbarb60 :: (Floating u, Real u, FromPtSize u) => Arrowhead u
revbarb60 = Arrowhead $ 
    intoThetaLocImage (rlift2 markHeight) (revbarbTLG (pi/3))

revbarb45 :: (Floating u, Real u, FromPtSize u) => Arrowhead u
revbarb45 = Arrowhead $ 
    intoThetaLocImage (rlift2 markHeight) (revbarbTLG (pi/4))


perpTLG :: (Floating u, FromPtSize u) => ThetaLocGraphic u
perpTLG = bindAskR2 markHalfHeight $ \hh ->
    \theta pt -> let p0 = displacePerpendicular   hh  theta pt
                     p1 = displacePerpendicular (-hh) theta pt  
                 in straightLineBetween p0 p1


perp :: (Floating u, FromPtSize u) => Arrowhead u
perp = Arrowhead $ intoThetaLocImage noRetract perpTLG



bracketTLG :: (Floating u, FromPtSize u) => ThetaLocGraphic u
bracketTLG = bindAskR2 markHalfHeight $ \hh -> 
    \theta pt -> let p1 = displacePerpendicular   hh  theta pt
                     p0 = displaceParallel      (-hh) theta p1
                     p2 = displacePerpendicular (-hh) theta pt
                     p3 = displaceParallel      (-hh) theta p2
                 in openStroke $ vertexPath [p0,p1,p2,p3]




bracket :: (Floating u, FromPtSize u) => Arrowhead u
bracket = Arrowhead $ intoThetaLocImage noRetract bracketTLG

diskTLG :: (Floating u, FromPtSize u) 
        => (u -> Point2 u -> Graphic u) -> ThetaLocGraphic u
diskTLG drawF = bindAskR2 markHalfHeight $ \hh ->
    \theta pt -> let ctr = pt .-^ avec theta hh
                 in drawF hh ctr

diskTip :: (Floating u, FromPtSize u) => Arrowhead u
diskTip = Arrowhead $ intoThetaLocImage (rlift2 markHeight) (diskTLG drawF)
  where
    drawF r pt = localize bothStrokeColour $ filledDisk r pt


odiskTip :: (Floating u, FromPtSize u) => Arrowhead u
odiskTip = Arrowhead $ intoThetaLocImage (rlift2 markHeight) (diskTLG drawF)
  where
    drawF r pt = strokedDisk r pt


squareTLG :: (Floating u, FromPtSize u) 
        => (PrimPath u -> Graphic u) -> ThetaLocGraphic u
squareTLG drawF = bindAskR2 markHalfHeight $ \hh ->
    \theta pt -> let p0 = displacePerpendicular     hh  theta pt
                     p3 = displacePerpendicular   (-hh) theta pt
                     p1 = displaceParallel      (-2*hh) theta p0
                     p2 = displaceParallel      (-2*hh) theta p3
                 in drawF $ vertexPath [p0,p1,p2,p3]

squareTip :: (Floating u, FromPtSize u) => Arrowhead u
squareTip = Arrowhead $ intoThetaLocImage (rlift2 markHeight) (squareTLG drawF)
  where
    drawF = localize bothStrokeColour . filledPath


osquareTip :: (Floating u, FromPtSize u) => Arrowhead u
osquareTip = Arrowhead $ 
    intoThetaLocImage (rlift2 markHeight) (squareTLG closedStroke)


diamondTLG :: (Floating u, FromPtSize u) 
        => (PrimPath u -> Graphic u) -> ThetaLocGraphic u
diamondTLG drawF = bindAskR2 markHalfHeight $ \hh ->
    \theta pt -> let ctr = displaceParallel       (-2*hh) theta pt
                     p1  = displacePerpendicular     hh   theta ctr
                     p3  = displacePerpendicular   (-hh)  theta ctr
                     p2  = displaceParallel       (-4*hh) theta pt
                 in drawF $ vertexPath [pt,p1,p2,p3]


diamondTip :: (Floating u, FromPtSize u) => Arrowhead u
diamondTip = Arrowhead $ 
    intoThetaLocImage (rlift2 $ fmap (2*) markHeightLessLineWidth) 
                      (diamondTLG drawF)
  where
    drawF = localize bothStrokeColour . filledPath


odiamondTip :: (Floating u, FromPtSize u) => Arrowhead u
odiamondTip = Arrowhead $ 
    intoThetaLocImage (rlift2 $ fmap (2*) markHeight) (diamondTLG closedStroke)
