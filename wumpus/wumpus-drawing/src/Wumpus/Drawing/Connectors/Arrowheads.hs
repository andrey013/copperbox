{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Drawing.Connectors.Arrowheads
-- Copyright   :  (c) Stephen Tetley 2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Arrowheads.
--
--------------------------------------------------------------------------------

module Wumpus.Drawing.Connectors.Arrowheads
  (

  

    tri90'
  , tri45'
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

  , diamondWideTip
  , odiamondWideTip

  , curveTip
  , revcurveTip

  ) where


import Wumpus.Drawing.Connectors.Base
import Wumpus.Drawing.Paths.Absolute

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace

import Control.Applicative



-- | Arrow tips are drawn with a sloid line even if the connector
-- line is dashed (tips also override round corners)

solid_stroke_tip :: DrawingContextF
solid_stroke_tip = reset_drawing_metrics


type PointGen = Radian -> [Vec2 En]


filledTipPath :: PointGen -> LocThetaGraphic En
filledTipPath fn = 
    localize fill_use_stroke_colour $ promoteR2 $ \pt theta ->
      let vs = fn theta in vertexPP (map (pt .+^) vs) >>= dcClosedPath STROKE -- FILL


closedTipPath :: PointGen -> LocThetaGraphic En
closedTipPath fn = 
    localize solid_stroke_tip $ promoteR2 $ \pt theta ->
      let vs = fn theta in vertexPP (map (pt .+^) vs) >>= dcClosedPath STROKE


openTipPath :: PointGen -> LocThetaGraphic En
openTipPath fn = 
    localize solid_stroke_tip $ promoteR2 $ \pt theta ->
      let vs = fn theta in vertexPP (map (pt .+^) vs) >>= dcOpenPath



-- | Return @o-a@ and @o-b@:
--
-- >    a
-- >    .\
-- >    . \
-- >  .....o
-- >    . /
-- >    ./
-- >    b
--
tripointsFromTip :: En -> Radian -> (Radian -> (Vec2 En, Vec2 En))
tripointsFromTip baselen ang = \theta -> 
    (avec (theta + au) hyp, avec (theta + ad) hyp)
  where
    half_ang  = 0.5 * ang 
    au        = pi - half_ang
    ad        = half_ang + pi
    hyp       = baselen / (fromRadian $ cos half_ang)



-- | Return @o-x@ and @o-a@ and @o-b@:
--
-- >       a
-- >      /.
-- >     / .
-- > .. x..o
-- >     \ .
-- >      \.
-- >       b
--
revTripointsFromTip :: En -> Radian -> (Radian -> (Vec2 En, Vec2 En, Vec2 En))
revTripointsFromTip baselen ang = \theta -> 
    (avec theta (-baselen), oa theta, ob theta)
  where
    half_ang    = 0.5 * ang 
    half_height = baselen * (fromRadian $ tan half_ang)
    oa          = \theta -> avec (theta + ang90) half_height
    ob          = \theta -> avec (theta - ang90) half_height




zeroVec :: Vec2 En
zeroVec = V2 0 0 

ang90 :: Radian
ang90 = pi / 2

ang60 :: Radian
ang60 = pi / 3

ang45 :: Radian
ang45 = pi / 4


len_one :: InterpretUnit u => Query u
len_one = uconvertCtx1 (1::En)

len_two :: InterpretUnit u => Query u
len_two = uconvertCtx1 (2::En)

len_half :: InterpretUnit u => Query u
len_half = uconvertCtx1 (0.5::En)

len_zero :: InterpretUnit u => Query u
len_zero = pure 0

retract_one :: InterpretUnit u => Query u
retract_one = uconvertCtx1 (1::En)

retract_two :: InterpretUnit u => Query u
retract_two = uconvertCtx1 (2::En)


-- retract_half :: InterpretUnit u => Query u
-- retract_half = uconvertCtx1 (0.5::En)

retract_zero :: InterpretUnit u => Query u
retract_zero = pure 0



filledTriangle :: Radian -> ArrowAlg
filledTriangle ang = 
    ArrowAlg
      { retract_distance = const 1
      , tip_half_len     = 0.5
      , tip_deco         = filledTipPath spec
      }
  where
    spec theta = let (v1,v2) = tripointsFromTip 1 ang theta 
                 in [zeroVec, v1, v2]


tri90' :: ArrowAlg
tri90' = filledTriangle ang90


tri45' :: ArrowAlg
tri45' = filledTriangle ang45


filledTri :: InterpretUnit u => Radian -> ArrowTip u
filledTri ang = 
    makeArrowTip retract_one len_one (filledTipPath spec)
  where
    spec theta = let (v1,v2) = tripointsFromTip 1 ang theta 
                 in [zeroVec, v1, v2]


tri90 :: InterpretUnit u => ArrowTip u
tri90 = filledTri ang90

tri60 :: InterpretUnit u => ArrowTip u
tri60 = filledTri ang60

tri45 :: InterpretUnit u => ArrowTip u
tri45 = filledTri ang45


strokedClosedTri :: InterpretUnit u => Radian -> ArrowTip u
strokedClosedTri ang = 
    makeArrowTip retract_one len_one (closedTipPath spec)
  where
    spec theta = let (v1,v2) = tripointsFromTip 1 ang theta 
                 in [zeroVec, v1, v2]


otri90 :: InterpretUnit u => ArrowTip u
otri90 = strokedClosedTri ang90

otri60 :: InterpretUnit u => ArrowTip u
otri60 = strokedClosedTri ang60

otri45 :: InterpretUnit u => ArrowTip u
otri45 = strokedClosedTri ang45


filledRevTri :: InterpretUnit u => Radian -> ArrowTip u
filledRevTri ang = 
    makeArrowTip retract_one len_one (filledTipPath spec)
  where
    spec theta = let (v0,v1,v2) = revTripointsFromTip 1 ang theta 
                 in [v0, v1, v2]

revtri90 :: InterpretUnit u => ArrowTip u
revtri90 = filledRevTri ang90

revtri60 :: InterpretUnit u => ArrowTip u
revtri60 = filledRevTri ang60

revtri45 :: InterpretUnit u => ArrowTip u
revtri45 = filledRevTri ang45


strokedClosedRevTri :: InterpretUnit u => Radian -> ArrowTip u
strokedClosedRevTri ang = 
    makeArrowTip retract_one len_one (closedTipPath spec)
  where
    spec theta = let (v0,v1,v2) = revTripointsFromTip 1 ang theta 
                 in [v0, v1, v2]


orevtri90 :: InterpretUnit u => ArrowTip u
orevtri90 = strokedClosedRevTri ang90

orevtri60 :: InterpretUnit u => ArrowTip u
orevtri60 = strokedClosedRevTri ang60

orevtri45 :: InterpretUnit u => ArrowTip u
orevtri45 = strokedClosedRevTri ang45



strokedBarb :: InterpretUnit u => Radian -> ArrowTip u
strokedBarb ang = 
    makeArrowTip retract_zero len_one (openTipPath spec)
  where
    spec theta = let (v1,v2) = tripointsFromTip 1 ang theta 
                 in [v1,zeroVec,v2]


barb90 :: InterpretUnit u => ArrowTip u
barb90 = strokedBarb ang90

barb60 :: InterpretUnit u => ArrowTip u
barb60 = strokedBarb ang60

barb45 :: InterpretUnit u => ArrowTip u
barb45 = strokedBarb ang45


strokedRevBarb :: InterpretUnit u => Radian -> ArrowTip u
strokedRevBarb ang = 
    makeArrowTip retract_one len_one (openTipPath spec)
  where
    spec theta = let (v0,v1,v2) = revTripointsFromTip 1 ang theta 
                 in [v1,v0,v2]

revbarb90 :: InterpretUnit u => ArrowTip u
revbarb90 = strokedRevBarb ang90

revbarb60 :: InterpretUnit u => ArrowTip u
revbarb60 = strokedRevBarb ang60

revbarb45 :: InterpretUnit u => ArrowTip u
revbarb45 = strokedRevBarb ang45



perp :: InterpretUnit u => ArrowTip u
perp = 
    makeArrowTip retract_zero len_zero (openTipPath spec)
  where
    spec theta = let oa = avec (theta + ang90) 0.5
                     ob = avec (theta - ang90) 0.5
                 in [oa, ob]


bracket :: InterpretUnit u => ArrowTip u
bracket = 
    makeArrowTip retract_zero len_half (openTipPath spec)
  where
    spec theta = let oa = avec (theta + ang90) 0.5
                     ob = avec (theta - ang90) 0.5
                     dv = avec theta (-0.5)
                 in [ oa ^+^ dv, oa, ob, ob ^+^ dv ]



diskTip :: InterpretUnit u => ArrowTip u
diskTip = 
    makeArrowTip retract_one len_one  
                 (promoteR2 $ \pt theta -> body theta `at` pt)
  where
    body :: Radian -> LocGraphic En
    body theta = let v1 = avec theta (-0.5)
                 in localize fill_use_stroke_colour $ 
                      moveStart (dispVec v1) (dcDisk FILL 0.5)


odiskTip :: InterpretUnit u => ArrowTip u
odiskTip = 
    makeArrowTip retract_one len_one 
                 (promoteR2 $ \pt theta -> body theta `at` pt)
  where
    body :: Radian -> LocGraphic En
    body theta = let v1 = avec theta (-0.5)
                 in localize solid_stroke_tip $ 
                      moveStart (dispVec v1) (dcDisk STROKE 0.5)


-- | squareSpec:
--
-- > 
-- >    ,-----a
-- >    |     |
-- > ...v.....o
-- >    |     | 
-- >    `-----b
--
squareSpec :: PointGen
squareSpec theta = [ oa ^+^ ov, oa, ob, ob ^+^ ov ]
  where
    oa = avec (theta + ang90) 0.5
    ob = avec (theta - ang90) 0.5
    ov = avec theta (-1)
    

squareTip :: InterpretUnit u => ArrowTip u
squareTip = 
    makeArrowTip retract_one len_one (filledTipPath squareSpec)

osquareTip :: InterpretUnit u => ArrowTip u
osquareTip = 
    makeArrowTip retract_one len_one (closedTipPath squareSpec)



-- | squareSpec:
--
-- > 
-- >       a
-- >     /   \
-- > ...w..v..o
-- >     \   / 
-- >       b
--
diamondSpec :: En -> PointGen
diamondSpec width theta = [ ow, oa, zeroVec, ob ]
  where
    ow = avec theta (-width)
    ov = avec theta (negate $ 0.5 * width)
    oa = ov ^+^ avec (theta + ang90) 0.5
    ob = ov ^+^ avec (theta - ang90) 0.5


diamondTip :: InterpretUnit u => ArrowTip u
diamondTip = 
    makeArrowTip retract_one len_one (filledTipPath $ diamondSpec 1)

odiamondTip :: InterpretUnit u => ArrowTip u
odiamondTip = 
    makeArrowTip retract_one len_one (closedTipPath $ diamondSpec 1)


diamondWideTip :: InterpretUnit u => ArrowTip u
diamondWideTip = 
    makeArrowTip retract_two len_two (filledTipPath $ diamondSpec 2)

odiamondWideTip :: InterpretUnit u => ArrowTip u
odiamondWideTip = 
    makeArrowTip retract_two len_two (closedTipPath $ diamondSpec 2)


curveTipPath :: Point2 En -> Radian -> AbsPath En
curveTipPath pt theta = 
    curve1 a b c pt `append` curve1 pt z y x
  where
    ow  = avec theta (-1)
    a   = pt .+^ ow ^+^ avec (theta + ang90) 0.5
    x   = pt .+^ ow ^+^ avec (theta - ang90) 0.5

    (c,b) = trapezoidFromBasePoints 0.125 0.5 pt a
    (y,z) = trapezoidFromBasePoints 0.125 0.5 x pt


curveTip :: InterpretUnit u => ArrowTip u
curveTip = 
    makeArrowTip retract_zero len_one
                 (promoteR2 $ \pt theta -> 
                    localize (join_bevel . solid_stroke_tip) $ 
                      toPrimPath (curveTipPath pt theta) >>= dcOpenPath)



curveTipRevPath :: Point2 En -> Radian -> AbsPath En
curveTipRevPath pt theta = 
    curve1 a b c p2 `append` curve1 p2 z y x
  where
    p2  = pt .+^ avec theta (-1)
    a   = pt .+^ avec (theta + ang90) 0.5
    x   = pt .+^ avec (theta - ang90) 0.5

    (b,c) = trapezoidFromBasePoints 0.125 0.5 a p2
    (z,y) = trapezoidFromBasePoints 0.125 0.5 p2 x


revcurveTip :: InterpretUnit u => ArrowTip u
revcurveTip = 
    makeArrowTip retract_one len_one
                 (promoteR2 $ \pt theta -> 
                    localize (join_bevel . solid_stroke_tip) $ 
                      toPrimPath (curveTipRevPath pt theta) >>= dcOpenPath)


    
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
    base_mid  = dispParallel (0.5 * base_len) theta p1
    ubase_mid = dispPerpendicular u theta base_mid
    cp1       = dispParallel (-half_ulen) theta ubase_mid
    cp2       = dispParallel   half_ulen  theta ubase_mid