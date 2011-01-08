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

import Wumpus.Basic.Kernel
import Wumpus.Drawing.Paths

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
newtype Arrowhead u = Arrowhead { getArrowhead :: LocThetaImage u u }




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
markHeightPlusLineWidth :: (Fractional u, FromPtSize u) => DrawingR u
markHeightPlusLineWidth = 
    (\h lw -> h + realToFrac lw) <$> markHeight <*> lineWidth
-}


markHeightLessLineWidth :: (Fractional u, FromPtSize u) => CF u
markHeightLessLineWidth = 
    (\h lw -> h - realToFrac lw) <$> markHeight <*> getLineWidth


-- noRetract ignores both the angle and the point.
--
-- Its common for the rectraction not to care about the angle or 
-- the point and only care about the DrawingCtx.
--
noRetract :: Num u => LocThetaCF u u
noRetract = promoteR2 $ \_ _ -> pure 0 



--------------------------------------------------------------------------------

tipBody :: FromPtSize u => (Point2 u -> Radian -> u -> CF a) -> LocThetaCF u a
tipBody mf = promoteR2 $ \pt theta -> markHeight >>= \h -> mf pt theta h 

-- | Tripoints takes the \*tip length\* is the mark height.
--
-- This means that the 90deg tip has a tip width greater-than the
-- mark height (but that is okay - seemingly this is how TikZ 
-- does it).
--
tripointsByAngle :: (Floating u, FromPtSize u)
                 => Radian -> LocThetaCF u (Point2 u, Point2 u)
tripointsByAngle triang = 
    tipBody $ \pt theta h -> 
      let (vup,vlo) = triVecsByAngle h (0.5*triang) theta
      in  pure (pt .+^ vup, pt .+^ vlo)
    

revtripointsByAngle :: (Floating u, FromPtSize u)
                    => Radian 
                    -> LocThetaCF u (Point2 u, Point2 u, Point2 u)
revtripointsByAngle triang = 
    tipBody $ \pt theta h -> 
      let theta'    = circularModulo $ pi+theta 
          (vup,vlo) = triVecsByAngle h (0.5*triang) theta'
          back_tip  = pt .-^ avec theta h 
      in pure (back_tip .+^ vup, back_tip, back_tip .+^ vlo)



tripointsByDist :: (Real u, Floating u, FromPtSize u)
                => LocThetaCF u (Point2 u, Point2 u)
tripointsByDist = 
    tipBody $ \pt theta h -> 
      let (vup,vlo) = triVecsByDist h (0.5*h) theta
      in pure (pt .+^ vup, pt .+^ vlo)
  


revtripointsByDist :: (Real u, Floating u, FromPtSize u)
                   => LocThetaCF u (Point2 u, Point2 u, Point2 u)
revtripointsByDist = 
    tipBody $ \pt theta h -> 
      let theta'    = circularModulo $ pi+theta 
          (vup,vlo) = triVecsByDist h (0.5*h) theta'
          back_tip  = pt .-^ avec theta h 
      in pure (back_tip .+^ vup, back_tip, back_tip .+^ vlo)





-- width = xchar_height
-- filled with stroke colour!

triTLG :: (Floating u, Real u, FromPtSize u)
       => Radian -> (PrimPath u -> Graphic u) -> LocThetaGraphic u
triTLG triang drawF = 
    promoteR2 $ \pt theta ->
      localize bothStrokeColour $ 
         apply2R2 (tripointsByAngle triang) pt theta >>= \(u,v) -> 
           drawF $ vertexPath [pt,u,v]



tri90 :: (Floating u, Real u, FromPtSize u) => Arrowhead u
tri90 = Arrowhead $ 
    intoLocThetaImage (lift0R2 markHeight) (triTLG (pi/2) filledPath)


tri60 :: (Floating u, Real u, FromPtSize u) => Arrowhead u
tri60 = Arrowhead $
    intoLocThetaImage (lift0R2 markHeight) (triTLG (pi/3) filledPath)


tri45 :: (Floating u, Real u, FromPtSize u) => Arrowhead u
tri45 = Arrowhead $ 
    intoLocThetaImage (lift0R2 markHeight) (triTLG (pi/4) filledPath)

otri90 :: (Floating u, Real u, FromPtSize u) => Arrowhead u
otri90 = Arrowhead $
    intoLocThetaImage (lift0R2 markHeight) (triTLG (pi/2) closedStroke)

otri60 :: (Floating u, Real u, FromPtSize u) => Arrowhead u
otri60 = Arrowhead $   
    intoLocThetaImage (lift0R2 markHeight) (triTLG (pi/3) closedStroke)

otri45 :: (Floating u, Real u, FromPtSize u) => Arrowhead u
otri45 = Arrowhead $ 
    intoLocThetaImage (lift0R2 markHeight) (triTLG (pi/4) closedStroke)


-- width = xchar_height
-- filled with stroke colour!

revtriTLG :: (Floating u, Real u, FromPtSize u)
          => Radian -> (PrimPath u -> Graphic u) -> LocThetaGraphic u
revtriTLG triang drawF = 
    promoteR2 $ \pt theta -> 
      localize bothStrokeColour $ 
        apply2R2 (revtripointsByAngle triang) pt theta >>= \(u,pt',v) -> 
           drawF $ vertexPath [u,pt',v]



revtri90 :: (Floating u, Real u, FromPtSize u) => Arrowhead u
revtri90 = Arrowhead $ 
    intoLocThetaImage (lift0R2 markHeightLessLineWidth) 
                      (revtriTLG (pi/2) filledPath)

revtri60 :: (Floating u, Real u, FromPtSize u) => Arrowhead u
revtri60 = Arrowhead $ 
    intoLocThetaImage (lift0R2 markHeightLessLineWidth) 
                      (revtriTLG (pi/3) filledPath)

revtri45 :: (Floating u, Real u, FromPtSize u) => Arrowhead u
revtri45 = Arrowhead $ 
    intoLocThetaImage (lift0R2 markHeightLessLineWidth) 
                      (revtriTLG (pi/4) filledPath)


orevtri90 :: (Floating u, Real u, FromPtSize u) => Arrowhead u
orevtri90 = Arrowhead $ 
    intoLocThetaImage (lift0R2 markHeightLessLineWidth) 
                      (revtriTLG (pi/2) closedStroke)

orevtri60 :: (Floating u, Real u, FromPtSize u) => Arrowhead u
orevtri60 = Arrowhead $ 
    intoLocThetaImage (lift0R2 markHeightLessLineWidth) 
                      (revtriTLG (pi/3) closedStroke)

orevtri45 :: (Floating u, Real u, FromPtSize u) => Arrowhead u
orevtri45 = Arrowhead $ 
    intoLocThetaImage (lift0R2 markHeightLessLineWidth) 
                      (revtriTLG (pi/4) closedStroke)



barbTLG :: (Floating u, Real u, FromPtSize u) => Radian -> LocThetaGraphic u
barbTLG ang =  
    promoteR2 $ \pt theta -> 
      apply2R2 (tripointsByAngle ang) pt theta >>= \(u,v) -> 
        openStroke $ vertexPath [u,pt,v]



barb90 :: (Floating u, Real u, FromPtSize u) => Arrowhead u
barb90 = Arrowhead $ intoLocThetaImage noRetract (barbTLG (pi/2))

barb60 :: (Floating u, Real u, FromPtSize u) => Arrowhead u
barb60 = Arrowhead $ intoLocThetaImage noRetract (barbTLG (pi/3))


barb45 :: (Floating u, Real u, FromPtSize u) => Arrowhead u
barb45 = Arrowhead $ intoLocThetaImage noRetract (barbTLG (pi/4))



revbarbTLG :: (Floating u, Real u, FromPtSize u) => Radian -> LocThetaGraphic u
revbarbTLG ang = 
    promoteR2 $ \pt theta -> 
      apply2R2 (revtripointsByAngle ang) pt theta >>= \(u,pt',v) -> 
        openStroke $ vertexPath [u,pt',v]

revbarb90 :: (Floating u, Real u, FromPtSize u) => Arrowhead u
revbarb90 = Arrowhead $ 
    intoLocThetaImage (lift0R2 markHeight) (revbarbTLG (pi/2))


revbarb60 :: (Floating u, Real u, FromPtSize u) => Arrowhead u
revbarb60 = Arrowhead $ 
    intoLocThetaImage (lift0R2 markHeight) (revbarbTLG (pi/3))

revbarb45 :: (Floating u, Real u, FromPtSize u) => Arrowhead u
revbarb45 = Arrowhead $ 
    intoLocThetaImage (lift0R2 markHeight) (revbarbTLG (pi/4))


perpTLG :: (Floating u, FromPtSize u) => LocThetaGraphic u
perpTLG = 
    tipBody $ \pt theta h -> 
      let hh = 0.5*h in openStroke $ rperpPath hh pt theta


rperpPath :: Floating u => u -> Point2 u -> Radian -> PrimPath u
rperpPath hh ctr theta = primPath p0 [lineTo p1]
  where
    p0 = displacePerpendicular   hh  theta ctr
    p1 = displacePerpendicular (-hh) theta ctr 
             


perp :: (Floating u, FromPtSize u) => Arrowhead u
perp = Arrowhead $ intoLocThetaImage noRetract perpTLG



bracketTLG :: (Floating u, FromPtSize u) => LocThetaGraphic u
bracketTLG = 
    tipBody $ \pt theta h -> 
      let hh = 0.5*h in openStroke $ rbracketPath hh pt theta


rbracketPath :: Floating u => u -> Point2 u -> Radian -> PrimPath u
rbracketPath hh pt theta = vertexPath [p0,p1,p2,p3]
  where
    p1 = displacePerpendicular   hh  theta pt
    p0 = displaceParallel      (-hh) theta p1
    p2 = displacePerpendicular (-hh) theta pt
    p3 = displaceParallel      (-hh) theta p2
        



bracket :: (Floating u, FromPtSize u) => Arrowhead u
bracket = Arrowhead $ intoLocThetaImage noRetract bracketTLG


diskTLG :: (Floating u, FromPtSize u) 
        => (u -> Point2 u -> Graphic u) -> LocThetaGraphic u
diskTLG drawF = 
    tipBody $ \pt theta h -> let hh  = 0.5*h 
                                 ctr = pt .-^ avec theta hh 
                             in drawF hh ctr


diskTip :: (Floating u, FromPtSize u) => Arrowhead u
diskTip = Arrowhead $ intoLocThetaImage (lift0R2 markHeight) (diskTLG drawF)
  where
    drawF r pt = localize bothStrokeColour $ filledDisk r `at` pt


odiskTip :: (Floating u, FromPtSize u) => Arrowhead u
odiskTip = Arrowhead $ intoLocThetaImage (lift0R2 markHeight) (diskTLG drawF)
  where
    drawF r pt = strokedDisk r `at` pt


squareTLG :: (Floating u, FromPtSize u) 
        => (PrimPath u -> Graphic u) -> LocThetaGraphic u
squareTLG drawF = 
    tipBody $ \pt theta h -> drawF $ rsquarePath pt theta (0.5*h)


rsquarePath :: Floating u => Point2 u -> Radian -> u -> PrimPath u
rsquarePath pt theta hh = vertexPath [p0,p1,p2,p3]
  where
    p0 = displacePerpendicular     hh  theta pt
    p3 = displacePerpendicular   (-hh) theta pt
    p1 = displaceParallel      (-2*hh) theta p0
    p2 = displaceParallel      (-2*hh) theta p3
    

squareTip :: (Floating u, FromPtSize u) => Arrowhead u
squareTip = Arrowhead $ intoLocThetaImage (lift0R2 markHeight) (squareTLG drawF)
  where
    drawF = localize bothStrokeColour . filledPath


osquareTip :: (Floating u, FromPtSize u) => Arrowhead u
osquareTip = Arrowhead $ 
    intoLocThetaImage (lift0R2 markHeight) (squareTLG closedStroke)


diamondTLG :: (Floating u, FromPtSize u) 
           => (PrimPath u -> Graphic u) -> LocThetaGraphic u
diamondTLG drawF = 
    tipBody $ \pt theta h -> drawF $ rdiamondPath pt theta (0.5*h)
 

rdiamondPath :: Floating u => Point2 u -> Radian -> u -> PrimPath u
rdiamondPath pt theta hh = vertexPath [pt,p1,p2,p3]
  where
    ctr = displaceParallel       (-2*hh) theta pt
    p1  = displacePerpendicular     hh   theta ctr
    p3  = displacePerpendicular   (-hh)  theta ctr
    p2  = displaceParallel       (-4*hh) theta pt
         


diamondTip :: (Floating u, FromPtSize u) => Arrowhead u
diamondTip = Arrowhead $ 
    intoLocThetaImage (lift0R2 $ fmap (2*) markHeightLessLineWidth) 
                      (diamondTLG drawF)
  where
    drawF = localize bothStrokeColour . filledPath


odiamondTip :: (Floating u, FromPtSize u) => Arrowhead u
odiamondTip = Arrowhead $ 
    intoLocThetaImage (lift0R2 $ fmap (2*) markHeight) (diamondTLG closedStroke)




-- Note - points flipped to get the second trapezium to 
-- draw /underneath/.
--
curveTLG :: (Real u, Floating u, FromPtSize u) => LocThetaGraphic u
curveTLG = 
    tipBody $ \pt theta h -> 
      cxCurvePath pt theta (0.5*h) >>= \path ->
        localize (joinRound . capRound) (openStroke path)


cxCurvePath :: (Real u, Floating u, FromPtSize u) 
            => Point2 u -> Radian -> u -> DrawingInfo (PrimPath u)
cxCurvePath pt theta hh =
     apply2R2 tripointsByDist pt theta >>= \(tup,tlo) -> 
          let (u1,u2) = trapezoidFromBasePoints (0.25*hh) 0.5 pt tup
              (l2,l1) = trapezoidFromBasePoints (0.25*hh) 0.5 tlo pt 
          in pure $ toPrimPath $ curve tup u2 u1 pt `append` curve pt l1 l2 tlo




curveTip :: (Real u, Floating u, FromPtSize u) => Arrowhead u
curveTip = Arrowhead $ 
    intoLocThetaImage (lift0R2 $ fmap realToFrac getLineWidth) curveTLG


-- Note - points flipped to get the second trapezium to 
-- draw /underneath/.
--
revcurveTLG :: (Real u, Floating u, FromPtSize u) => LocThetaGraphic u
revcurveTLG = 
    tipBody $ \pt theta h ->
      cxRevcurvePath pt theta (0.5*h) >>= \path ->
        localize (joinRound . capRound) (openStroke path)

cxRevcurvePath :: (Real u, Floating u, FromPtSize u) 
               => Point2 u -> Radian -> u -> DrawingInfo (PrimPath u)
cxRevcurvePath pt theta hh = 
    apply2R2 revtripointsByDist pt theta >>= \(tup,p1,tlo) -> 
      let (u1,u2) = trapezoidFromBasePoints (0.25*hh) 0.5 p1 tup
          (l2,l1) = trapezoidFromBasePoints (0.25*hh) 0.5 tlo p1
      in pure$ toPrimPath $ curve tup u2 u1 p1 `append` curve p1 l1 l2 tlo


revcurveTip :: (Real u, Floating u, FromPtSize u) => Arrowhead u
revcurveTip = Arrowhead $ 
    intoLocThetaImage (lift0R2 markHeight) revcurveTLG
