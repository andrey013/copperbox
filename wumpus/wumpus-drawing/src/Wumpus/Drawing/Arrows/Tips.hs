{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Arrows.Tips
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

module Wumpus.Drawing.Arrows.Tips
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

  , curveTip
  , revcurveTip

  ) where

import Wumpus.Drawing.Paths.Absolute

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space

import Control.Applicative



-- | Encode an arrowhead as a Graphic and a retract distance - 
-- lines should be shortened for certain drawings (e.g. open
-- triangles).
--
-- The retract distance is context sensitive - usually just on
-- the markHeight (or halfMarkHeight) so it has to be calculated
-- w.r.t. the DrawingCtx.
--
data Arrowhead u = Arrowhead 
      { arrowhead_retract_dist  :: Query u
      , arrowhead_draw          :: LocThetaGraphic u 
      }


-- Design note - this used to be a newtype wrapper over a 
-- LocThetaImage that returned retract distance. But, considering 
-- the dataflow / evaluation, in some respects Arrowhead is not an 
-- ideal LocThetaImage as we want the retract distance to work out 
-- how to draw the image.
--
-- Images do not inherently encode objects that should be drawn 
-- and evaluated at the same time (and lazy eval permits this) but 
-- it seems to helpful to think that Images should be evaluated as 
-- they are drawn.
--


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




{-
markHeightPlusLineWidth :: (Fractional u, InterpretUnit u) => DrawingR u
markHeightPlusLineWidth = 
    (\h lw -> h + realToFrac lw) <$> markHeight <*> lineWidth
-}


markHeightLessLineWidth :: (Fractional u, InterpretUnit u) => Query u
markHeightLessLineWidth = 
    (\h lw -> h - realToFrac lw) <$> markHeight <*> getLineWidth


-- noRetract ignores both the angle and the point.
--
-- Its common for the rectraction not to care about the angle or 
-- the point and only care about the DrawingCtx.
--
noRetract :: Num u => Query u
noRetract = pure 0 


-- | Arrow tips are drawn with a sloid line even if the connector
-- line is dashed (tips also override round corners)

solidArrTip :: DrawingCtxM m => m a -> m a
solidArrTip mf = localize reset_drawing_metrics mf


solidOpenStroke :: Num  u => PrimPath -> Graphic u
solidOpenStroke = solidArrTip . openStroke

solidClosedStroke :: Num  u => PrimPath -> Graphic  u
solidClosedStroke = solidArrTip . closedStroke

solidStrokedDisk :: InterpretUnit u => u -> LocGraphic  u
solidStrokedDisk = solidArrTip . strokedDisk

--------------------------------------------------------------------------------

tipBody :: InterpretUnit u 
        => (Point2 u -> Radian -> u -> Image u (t u)) -> LocThetaImage u (t u)
tipBody mf = promoteR2 $ \pt theta -> 
             markHeight >>= \h -> mf pt theta h 


tipQuery :: InterpretUnit u 
         => (Point2 u -> Radian -> u -> Query a) -> LocThetaQuery u a
tipQuery mf = promoteR2 $ \pt theta -> 
              markHeight >>= \h -> mf pt theta h 


-- | Tripoints takes the \*tip length\* is the mark height.
--
-- This means that the 90deg tip has a tip width greater-than the
-- mark height (but that is okay - seemingly this is how TikZ 
-- does it).
--
tripointsByAngle :: (Floating u, InterpretUnit u)
                 => Radian -> LocThetaQuery u (Point2 u, Point2 u)
tripointsByAngle triang = 
    tipQuery $ \pt theta h -> 
      let (vup,vlo) = triVecsByAngle h (0.5*triang) theta
      in  pure (pt .+^ vup, pt .+^ vlo)
    

revtripointsByAngle :: (Floating u, InterpretUnit u)
                    => Radian 
                    -> LocThetaQuery u (Point2 u, Point2 u, Point2 u)
revtripointsByAngle triang = 
    tipQuery $ \pt theta h -> 
      let theta'    = circularModulo $ pi+theta 
          (vup,vlo) = triVecsByAngle h (0.5*triang) theta'
          back_tip  = pt .-^ avec theta h 
      in pure (back_tip .+^ vup, back_tip, back_tip .+^ vlo)



tripointsByDist :: (Real u, Floating u, InterpretUnit u)
                => LocThetaQuery u (Point2 u, Point2 u)
tripointsByDist = 
    tipQuery $ \pt theta h -> 
      let (vup,vlo) = triVecsByDist h (0.5*h) theta
      in pure (pt .+^ vup, pt .+^ vlo)
  


revtripointsByDist :: (Real u, Floating u, InterpretUnit u)
                   => LocThetaQuery u (Point2 u, Point2 u, Point2 u)
revtripointsByDist = 
    tipQuery $ \pt theta h -> 
      let theta'    = circularModulo $ pi+theta 
          (vup,vlo) = triVecsByDist h (0.5*h) theta'
          back_tip  = pt .-^ avec theta h 
      in pure (back_tip .+^ vup, back_tip, back_tip .+^ vlo)





-- width = xchar_height
-- filled with stroke colour!

triTLG :: (Floating u, Real u, InterpretUnit u)
       => Radian -> (PrimPath -> Graphic u) -> LocThetaGraphic u
triTLG triang drawF = 
    promoteR2 $ \pt theta ->
      localize fill_use_stroke_colour $ 
        apply2R2 (tripointsByAngle triang) pt theta >>= \(u,v) -> 
        vertexPP [pt,u,v] >>= drawF



tri90 :: (Floating u, Real u, InterpretUnit u) => Arrowhead u
tri90 = Arrowhead markHeight (triTLG (pi/2) filledPath)


tri60 :: (Floating u, Real u, InterpretUnit u) => Arrowhead u
tri60 = Arrowhead markHeight (triTLG (pi/3) filledPath)


tri45 :: (Floating u, Real u, InterpretUnit u) => Arrowhead u
tri45 = Arrowhead markHeight (triTLG (pi/4) filledPath)

otri90 :: (Floating u, Real u, InterpretUnit u) => Arrowhead u
otri90 = Arrowhead markHeight (triTLG (pi/2) solidClosedStroke)

otri60 :: (Floating u, Real u, InterpretUnit u) => Arrowhead u
otri60 = Arrowhead markHeight (triTLG (pi/3) solidClosedStroke)

otri45 :: (Floating u, Real u, InterpretUnit u) => Arrowhead u
otri45 = Arrowhead markHeight (triTLG (pi/4) solidClosedStroke)


-- width = xchar_height
-- filled with stroke colour!

revtriTLG :: (Floating u, Real u, InterpretUnit u)
          => Radian -> (PrimPath -> Graphic u) -> LocThetaGraphic u
revtriTLG triang drawF = 
    promoteR2 $ \pt theta -> 
      localize fill_use_stroke_colour $ 
        apply2R2 (revtripointsByAngle triang) pt theta >>= \(u,pt',v) -> 
        vertexPP [u,pt',v] >>= drawF



revtri90 :: (Floating u, Real u, InterpretUnit u) => Arrowhead u
revtri90 = Arrowhead markHeightLessLineWidth
                     (revtriTLG (pi/2) filledPath)

revtri60 :: (Floating u, Real u, InterpretUnit u) => Arrowhead u
revtri60 = Arrowhead markHeightLessLineWidth
                     (revtriTLG (pi/3) filledPath)

revtri45 :: (Floating u, Real u, InterpretUnit u) => Arrowhead u
revtri45 = Arrowhead markHeightLessLineWidth
                     (revtriTLG (pi/4) filledPath)


orevtri90 :: (Floating u, Real u, InterpretUnit u) => Arrowhead u
orevtri90 = Arrowhead markHeightLessLineWidth
                      (revtriTLG (pi/2) solidClosedStroke)

orevtri60 :: (Floating u, Real u, InterpretUnit u) => Arrowhead u
orevtri60 = Arrowhead markHeightLessLineWidth
                      (revtriTLG (pi/3) solidClosedStroke)

orevtri45 :: (Floating u, Real u, InterpretUnit u) => Arrowhead u
orevtri45 = Arrowhead markHeightLessLineWidth
                      (revtriTLG (pi/4) solidClosedStroke)



barbTLG :: (Floating u, Real u, InterpretUnit u) 
        => Radian -> LocThetaGraphic u
barbTLG ang =  
    promoteR2 $ \pt theta -> 
      apply2R2 (tripointsByAngle ang) pt theta >>= \(u,v) -> 
      vertexPP [u,pt,v] >>= solidOpenStroke



barb90 :: (Floating u, Real u, InterpretUnit u) => Arrowhead u
barb90 = Arrowhead noRetract (barbTLG (pi/2))

barb60 :: (Floating u, Real u, InterpretUnit u) => Arrowhead u
barb60 = Arrowhead noRetract (barbTLG (pi/3))


barb45 :: (Floating u, Real u, InterpretUnit u) => Arrowhead u
barb45 = Arrowhead noRetract (barbTLG (pi/4))



revbarbTLG :: (Floating u, Real u, InterpretUnit u) 
           => Radian -> LocThetaGraphic u
revbarbTLG ang = 
    promoteR2 $ \pt theta -> 
      apply2R2 (revtripointsByAngle ang) pt theta >>= \(u,pt',v) -> 
      vertexPP [u,pt',v] >>= solidOpenStroke

revbarb90 :: (Floating u, Real u, InterpretUnit u) => Arrowhead u
revbarb90 = Arrowhead markHeight (revbarbTLG (pi/2))


revbarb60 :: (Floating u, Real u, InterpretUnit u) => Arrowhead u
revbarb60 = Arrowhead markHeight (revbarbTLG (pi/3))

revbarb45 :: (Floating u, Real u, InterpretUnit u) => Arrowhead u
revbarb45 = Arrowhead markHeight (revbarbTLG (pi/4))


perpTLG :: (Floating u, InterpretUnit u) => LocThetaGraphic u
perpTLG = 
    tipBody $ \pt theta h -> 
      let hh = 0.5*h in rperpPath hh pt theta >>= solidOpenStroke


rperpPath :: (Floating u, InterpretUnit u) 
          => u -> Point2 u -> Radian -> Query PrimPath
rperpPath hh ctr theta = vertexPP [p0,p1]
  where
    p0 = displacePerpendicular   hh  theta ctr
    p1 = displacePerpendicular (-hh) theta ctr 
             


perp :: (Floating u, InterpretUnit u) => Arrowhead u
perp = Arrowhead noRetract perpTLG



bracketTLG :: (Floating u, InterpretUnit u) => LocThetaGraphic u
bracketTLG = 
    tipBody $ \pt theta h -> 
      let hh = 0.5*h in rbracketPath hh pt theta >>= solidOpenStroke


rbracketPath :: (Floating u, InterpretUnit u) 
             => u -> Point2 u -> Radian -> Query PrimPath
rbracketPath hh pt theta = vertexPP [p0,p1,p2,p3]
  where
    p1 = displacePerpendicular   hh  theta pt
    p0 = displaceParallel      (-hh) theta p1
    p2 = displacePerpendicular (-hh) theta pt
    p3 = displaceParallel      (-hh) theta p2
        



bracket :: (Floating u, InterpretUnit u) => Arrowhead u
bracket = Arrowhead noRetract bracketTLG


diskTLG :: (Floating u, InterpretUnit u) 
        => (u -> Point2 u -> Graphic u) -> LocThetaGraphic u
diskTLG drawF = 
    tipBody $ \pt theta h -> let hh  = 0.5*h 
                                 ctr = pt .-^ avec theta hh 
                             in drawF hh ctr


diskTip :: (Floating u, InterpretUnit u) => Arrowhead u
diskTip = Arrowhead markHeight (diskTLG drawF)
  where
    drawF r pt = localize fill_use_stroke_colour $ filledDisk r `at` pt


odiskTip :: (Floating u, InterpretUnit u) => Arrowhead u
odiskTip = Arrowhead markHeight (diskTLG drawF)
  where
    drawF r pt = solidStrokedDisk r `at` pt


squareTLG :: (Floating u, InterpretUnit u) 
          => (PrimPath -> Graphic u) -> LocThetaGraphic u
squareTLG drawF = 
    tipBody $ \pt theta h -> rsquarePath pt theta (0.5*h) >>= drawF


rsquarePath :: (Floating u, InterpretUnit u) 
            => Point2 u -> Radian -> u -> Query PrimPath
rsquarePath pt theta hh = vertexPP [p0,p1,p2,p3]
  where
    p0 = displacePerpendicular     hh  theta pt
    p3 = displacePerpendicular   (-hh) theta pt
    p1 = displaceParallel      (-2*hh) theta p0
    p2 = displaceParallel      (-2*hh) theta p3
    

squareTip :: (Floating u, InterpretUnit u) => Arrowhead u
squareTip = Arrowhead markHeight (squareTLG drawF)
  where
    drawF = localize fill_use_stroke_colour . filledPath


osquareTip :: (Floating u, InterpretUnit u) => Arrowhead u
osquareTip = Arrowhead markHeight (squareTLG solidClosedStroke)


diamondTLG :: (Floating u, InterpretUnit u) 
           => (PrimPath -> Graphic u) -> LocThetaGraphic u
diamondTLG drawF = 
    tipBody $ \pt theta h -> 
    rdiamondPath pt theta (0.5*h) >>= drawF
 

rdiamondPath :: (Floating u, InterpretUnit u) 
             => Point2 u -> Radian -> u -> Query PrimPath
rdiamondPath pt theta hh = vertexPP [pt,p1,p2,p3]
  where
    ctr = displaceParallel       (-2*hh) theta pt
    p1  = displacePerpendicular     hh   theta ctr
    p3  = displacePerpendicular   (-hh)  theta ctr
    p2  = displaceParallel       (-4*hh) theta pt
         


diamondTip :: (Floating u, InterpretUnit u) => Arrowhead u
diamondTip = Arrowhead (fmap (2*) markHeightLessLineWidth) 
                       (diamondTLG drawF)
  where
    drawF = localize fill_use_stroke_colour . filledPath


odiamondTip :: (Floating u, InterpretUnit u) 
            => Arrowhead u
odiamondTip = 
    Arrowhead ((2*) <$> markHeight) (diamondTLG solidClosedStroke)




-- Note - points flipped to get the second trapezium to 
-- draw /underneath/.
--
curveTLG :: (Real u, Floating u, InterpretUnit u, Tolerance u) 
         => LocThetaGraphic u
curveTLG = 
    tipBody $ \pt theta h -> 
      cxCurvePath pt theta (0.5*h) >>= \path ->
        localize (join_round . cap_round) (solidOpenStroke path)


cxCurvePath :: (Real u, Floating u, InterpretUnit u, Tolerance u) 
            => Point2 u -> Radian -> u -> Query PrimPath
cxCurvePath pt theta hh =
    apply2R2 tripointsByDist pt theta >>= \(tup,tlo) -> 
      let (u1,u2) = trapezoidFromBasePoints (0.25*hh) 0.5 pt tup
          (l2,l1) = trapezoidFromBasePoints (0.25*hh) 0.5 tlo pt 
      in toPrimPath $ curve tup u2 u1 pt `append` curve pt l1 l2 tlo




curveTip :: (Real u, Floating u, InterpretUnit u, Tolerance u) 
         => Arrowhead u
curveTip = Arrowhead (realToFrac <$> getLineWidth) curveTLG


-- Note - points flipped to get the second trapezium to 
-- draw /underneath/.
--
revcurveTLG :: (Real u, Floating u, InterpretUnit u, Tolerance u) 
            => LocThetaGraphic u
revcurveTLG = 
    tipBody $ \pt theta h ->
      cxRevcurvePath pt theta (0.5*h) >>= \path ->
        localize (join_round . cap_round) (solidOpenStroke path)

cxRevcurvePath :: (Real u, Floating u, InterpretUnit u, Tolerance u) 
               => Point2 u -> Radian -> u -> Query PrimPath
cxRevcurvePath pt theta hh = 
    apply2R2 revtripointsByDist pt theta >>= \(tup,p1,tlo) -> 
      let (u1,u2) = trapezoidFromBasePoints (0.25*hh) 0.5 p1 tup
          (l2,l1) = trapezoidFromBasePoints (0.25*hh) 0.5 tlo p1
      in toPrimPath $ curve tup u2 u1 p1 `append` curve p1 l1 l2 tlo


revcurveTip :: (Real u, Floating u, InterpretUnit u, Tolerance u) 
            => Arrowhead u
revcurveTip = Arrowhead markHeight revcurveTLG



-- | 'trapezoidFromBasePoints' : 
-- @ altitude * ratio_to_base * start_pt * end_pt -> (top_left, top_right) @
--
-- Control points form an isosceles trapezoid.
--
-- The two manufactured control points form the top corners, 
-- so the supplied points map as @start_point == bottom_left@ and 
-- @end_point == bottom_right@.
-- 
trapezoidFromBasePoints :: (Real u, Floating u) 
                        => u -> u -> Point2 u -> Point2 u 
                        -> (Point2 u, Point2 u) 
trapezoidFromBasePoints u ratio_to_base p1 p2 = (cp1, cp2)
  where
    base_vec  = pvec p1 p2
    base_len  = vlength base_vec
    theta     = vdirection base_vec
    half_ulen = 0.5 * ratio_to_base * base_len
    base_mid  = displaceParallel (0.5 * base_len) theta p1
    ubase_mid = displacePerpendicular u theta base_mid
    cp1       = displaceParallel (-half_ulen) theta ubase_mid
    cp2       = displaceParallel   half_ulen  theta ubase_mid