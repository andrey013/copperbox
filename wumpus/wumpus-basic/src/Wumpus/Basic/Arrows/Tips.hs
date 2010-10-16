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

  , barb90
  , barb60
  , barb45

  , perp

  , rbracket

  ) where

import Wumpus.Basic.Graphic

import Wumpus.Core                      -- package: wumpus-core

import Data.AffineSpace                 -- package: vector-space

import Control.Applicative


-- This new formulation seems preferable...
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




markHeightPlusLineWidth :: (Fractional u, FromPtSize u) => DrawingR u
markHeightPlusLineWidth = 
    (\h lw -> h + realToFrac lw) <$> markHeight <*> lineWidth


-- noRetract ignores both the angle and the point.
--
-- Its common for the rectraction not to care about the angle or 
-- the point and only care about the DrawingCtx.
--
noRetract :: Num u => ThetaLocDrawingR u u
noRetract _ _ = pure 0 

-- Here\'s a lifter to ignore angle and point...
--
queryCtx :: DrawingR a -> ThetaLocDrawingR u a
queryCtx af = \_ _ -> af

--------------------------------------------------------------------------------

-- | Tripoints takes the \*tip length\* is the mark height.
--
-- This means that the 90deg tip has a tip width greater-than the
-- mark height (but that is okay - seemingly this is how TikZ 
-- does it!).
--
tripointsByAngle :: (Floating u, FromPtSize u)
                 => Radian ->  ThetaLocDrawingR u (Point2 u, Point2 u)
tripointsByAngle triang theta tip = 
    (\h -> let (vupper,vlower) = triVecsByAngle h (0.5*triang) theta
           in  (tip .+^ vupper, tip .+^ vlower))
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

triAng :: (Floating u, Real u, FromPtSize u)
       => Radian -> (PrimPath u -> Graphic u) -> ThetaLocGraphic u
triAng triang gf theta pt = 
    tripointsByAngle triang theta pt >>= \(u,v) -> 
    localize bothStrokeColour (gf $  vertexPath [pt,u,v])





tri90 :: (Floating u, Real u, FromPtSize u) => Arrowhead u
tri90 = Arrowhead $ 
    intoThetaLocImage (queryCtx markHeight) (triAng (pi/2) filledPath)


tri60 :: (Floating u, Real u, FromPtSize u) => Arrowhead u
tri60 = Arrowhead $
    intoThetaLocImage (queryCtx markHeight) (triAng (pi/3) filledPath)


tri45 :: (Floating u, Real u, FromPtSize u) => Arrowhead u
tri45 = Arrowhead $ 
    intoThetaLocImage (queryCtx markHeight) (triAng (pi/4) filledPath)

otri90 :: (Floating u, Real u, FromPtSize u) => Arrowhead u
otri90 = Arrowhead $
    intoThetaLocImage (queryCtx markHeightPlusLineWidth) 
                      (triAng (pi/2) closedStroke)

otri60 :: (Floating u, Real u, FromPtSize u) => Arrowhead u
otri60 = Arrowhead $   
    intoThetaLocImage (queryCtx markHeightPlusLineWidth) 
                      (triAng (pi/3) closedStroke)

otri45 :: (Floating u, Real u, FromPtSize u) => Arrowhead u
otri45 = Arrowhead $ 
    intoThetaLocImage (queryCtx markHeightPlusLineWidth) 
                      (triAng (pi/4) closedStroke)



barbAng :: (Floating u, Real u, FromPtSize u) => Radian -> ThetaLocGraphic u
barbAng ang theta pt = 
    tripointsByAngle ang theta pt >>= \(u,v) -> 
    openStroke (vertexPath [u,pt,v])


barb90 :: (Floating u, Real u, FromPtSize u) => Arrowhead u
barb90 = Arrowhead $ intoThetaLocImage noRetract (barbAng (pi/2))

barb60 :: (Floating u, Real u, FromPtSize u) => Arrowhead u
barb60 = Arrowhead $ intoThetaLocImage noRetract (barbAng (pi/3))


barb45 :: (Floating u, Real u, FromPtSize u) => Arrowhead u
barb45 = Arrowhead $ intoThetaLocImage noRetract (barbAng (pi/4))



perpAng :: (Floating u, FromPtSize u) => ThetaLocGraphic u
perpAng theta pt =  
    markHeight >>= \ h -> 
    let v = makeV h in openStroke $ vertexPath [ pt .+^ v, pt .-^ v]
  where
    makeV h  = avec (theta + pi/2) (0.5 * h)


perp :: (Floating u, FromPtSize u) => Arrowhead u
perp = Arrowhead $ intoThetaLocImage noRetract perpAng

rbracketAng :: (Floating u, FromPtSize u) => ThetaLocGraphic u
rbracketAng theta pt = markHalfHeight >>= \hh -> 
   runDirection theta $ 
     displacePerp   hh  pt >>= \p1 ->
     displacePara (-hh) p1 >>= \p0 ->
     displacePerp (-hh) pt >>= \p2 ->
     displacePara (-hh) p2 >>= \p3 ->
     return $ openStroke $ vertexPath [p0,p1,p2,p3]
   

rbracket :: (Floating u, FromPtSize u) => Arrowhead u
rbracket = Arrowhead $ intoThetaLocImage noRetract rbracketAng