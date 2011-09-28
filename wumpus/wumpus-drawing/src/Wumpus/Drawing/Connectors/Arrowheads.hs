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

    tri90
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


import Wumpus.Drawing.Basis.DrawingPrimitives
import Wumpus.Drawing.Connectors.Base

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace




-- | Arrow tips are drawn with a sloid line even if the connector
-- line is dashed (tips also override round corners)

solid_stroke_tip :: DrawingContextF
solid_stroke_tip = reset_drawing_metrics


-- NOTE - PointGen is pending replacement by Trails...

type PointGen = Radian -> [Vec2 En]

type TrailGen = Radian -> AnaTrail En


filledTipPath :: PointGen -> LocThetaGraphic En
filledTipPath gen = 
    localize fill_use_stroke_colour $ promoteLocTheta $ \pt theta ->
      cStraightLines DRAW_FILL $ map (pt .+^) $ gen theta


closedTipPath :: PointGen -> LocThetaGraphic En
closedTipPath gen = 
    localize solid_stroke_tip $ promoteLocTheta $ \pt theta ->
      cStraightLines DRAW_STROKE $ map (pt .+^) $ gen theta


openTipPath :: PointGen -> LocThetaGraphic En
openTipPath gen = 
    localize solid_stroke_tip $ promoteLocTheta $ \pt theta ->
      oStraightLines $ map (pt .+^) $ gen theta


fillTrailTip :: TrailGen -> LocThetaGraphic En
fillTrailTip gen_pt = 
    localize fill_use_stroke_colour $ promoteLocTheta $ \pt theta ->
      supplyLoc pt $ drawAnaTrail CFILL $ gen_pt theta

closedTrailTip :: TrailGen -> LocThetaGraphic En
closedTrailTip gen_pt = 
    localize solid_stroke_tip $ promoteLocTheta $ \pt theta ->
      supplyLoc pt $ drawAnaTrail CSTROKE $ gen_pt theta


openTrailTip :: TrailGen -> LocThetaGraphic En
openTrailTip gen_pt = 
    localize solid_stroke_tip $ promoteLocTheta $ \pt theta ->
      supplyLoc pt $ drawAnaTrail OSTROKE $ gen_pt theta





-- | All three lines are stated.
--
closedTriTrail :: Radian -> TrailGen
closedTriTrail ang theta = 
    anaCatTrail (vreverse v1) (catline v1 <> catline v2 <> catline v3) 
  where
    half_ang = 0.5 * ang
    half_h   = 1.0 * (fromRadian $ tan half_ang)   -- 1.0 is base_width
    v1       = theta_adj_grazing 1 half_ang theta
    v2       = theta_bkwd_adj_grazing 1 half_ang theta
    v3       = theta_up (2 * half_h) theta


-- | All three lines are stated.
--
revClosedTriTrail :: Radian -> TrailGen
revClosedTriTrail ang theta = 
    anaCatTrail (vbkwd ^+^ vreverse v1) 
                  (catline v1 <> catline v2 <> catline v3) 
  where
    half_ang = 0.5 * ang
    half_h   = 1.0 * (fromRadian $ tan half_ang)   -- 1.0 is base_width
    v1       = theta_bkwd_adj_grazing 1 half_ang theta
    v2       = theta_adj_grazing 1 half_ang theta
    v3       = theta_up (2 * half_h) theta
    vbkwd    = theta_left 1.0 theta


filledTri :: Radian -> ArrowTip
filledTri ang = 
    ArrowTip
      { retract_distance = const 1
      , tip_half_len     = 0.5
      , tip_deco         = fillTrailTip $ closedTriTrail ang
      }

-- | Filled triangle - apex is 90 deg.
--
tri90 :: ArrowTip
tri90 = filledTri ang90

-- | Filled triangle - apex is 60 deg.
--
tri60 :: ArrowTip
tri60 = filledTri ang60

-- | Filled triangle - apex is 45 deg.
--
tri45 :: ArrowTip
tri45 = filledTri ang45


strokedClosedTri :: Radian -> ArrowTip
strokedClosedTri ang = 
    ArrowTip
      { retract_distance = const 1
      , tip_half_len     = 0.5
      , tip_deco         = closedTrailTip $ closedTriTrail ang
      }


otri90 :: ArrowTip
otri90 = strokedClosedTri ang90

otri60 :: ArrowTip
otri60 = strokedClosedTri ang60

otri45 :: ArrowTip
otri45 = strokedClosedTri ang45


filledRevTri :: Radian -> ArrowTip
filledRevTri ang = 
    ArrowTip
      { retract_distance = const 1
      , tip_half_len     = 0.5
      , tip_deco         = fillTrailTip $ revClosedTriTrail ang
      }


revtri90 :: ArrowTip
revtri90 = filledRevTri ang90

revtri60 :: ArrowTip
revtri60 = filledRevTri ang60

revtri45 :: ArrowTip
revtri45 = filledRevTri ang45


strokedClosedRevTri :: Radian -> ArrowTip
strokedClosedRevTri ang = 
    ArrowTip
      { retract_distance = const 1
      , tip_half_len     = 0.5
      , tip_deco         = closedTrailTip $ revClosedTriTrail ang
      }

orevtri90 :: ArrowTip
orevtri90 = strokedClosedRevTri ang90

orevtri60 :: ArrowTip
orevtri60 = strokedClosedRevTri ang60

orevtri45 :: ArrowTip
orevtri45 = strokedClosedRevTri ang45


strokedBarb :: Radian -> ArrowTip
strokedBarb ang = 
    ArrowTip
      { retract_distance = const 0
      , tip_half_len     = 0.5
      , tip_deco         = openTrailTip spec
      }
  where
    half_ang   = 0.5 * ang
    spec theta = let v1 = theta_adj_grazing 1 half_ang theta
                     v2 = theta_bkwd_adj_grazing 1 half_ang theta
                 in anaCatTrail (vreverse v1) (catline v1 <> catline v2) 


barb90 :: ArrowTip
barb90 = strokedBarb ang90

barb60 :: ArrowTip
barb60 = strokedBarb ang60

barb45 :: ArrowTip
barb45 = strokedBarb ang45




strokedRevBarb :: Radian -> ArrowTip
strokedRevBarb ang = 
    ArrowTip
      { retract_distance = const 1
      , tip_half_len     = 0.5
      , tip_deco         = openTrailTip spec
      }
  where
    half_ang   = 0.5 * ang
    spec theta = let v1    = theta_bkwd_adj_grazing 1 half_ang theta
                     v2    = theta_adj_grazing 1 half_ang theta
                     vbkwd = theta_left 1 theta
                 in anaCatTrail (vbkwd ^+^ vreverse v1) 
                                (catline v1 <> catline v2) 


revbarb90 :: ArrowTip
revbarb90 = strokedRevBarb ang90

revbarb60 :: ArrowTip
revbarb60 = strokedRevBarb ang60

revbarb45 :: ArrowTip
revbarb45 = strokedRevBarb ang45



perp :: ArrowTip
perp = 
    ArrowTip
      { retract_distance = const 0
      , tip_half_len     = 0
      , tip_deco         = openTipPath spec
      }
  where
    spec theta = let oa = avec (theta + ang90) 0.5
                     ob = avec (theta - ang90) 0.5
                 in [oa, ob]


bracket :: ArrowTip
bracket = 
    ArrowTip
      { retract_distance = const 0
      , tip_half_len     = 0.5
      , tip_deco         = openTipPath spec
      }
  where
    spec theta = let oa = avec (theta + ang90) 0.5
                     ob = avec (theta - ang90) 0.5
                     dv = avec theta (-0.5)
                 in [ oa ^+^ dv, oa, ob, ob ^+^ dv ]



diskTip :: ArrowTip
diskTip = 
    ArrowTip
      { retract_distance = const 1
      , tip_half_len     = 0.5
      , tip_deco         = promoteLocTheta $ \pt theta -> body theta `at` pt
      }
  where
    body :: Radian -> LocGraphic En
    body theta = let v1 = avec theta (-0.5)
                 in localize fill_use_stroke_colour $ 
                      moveStart v1 (dcDisk DRAW_FILL 0.5)


odiskTip :: ArrowTip
odiskTip = 
    ArrowTip
      { retract_distance = const 1
      , tip_half_len     = 0.5
      , tip_deco         = promoteLocTheta $ \pt theta -> body theta `at` pt
      }
  where
    body :: Radian -> LocGraphic En
    body theta = let v1 = avec theta (-0.5)
                 in localize solid_stroke_tip $ 
                      moveStart v1 (dcDisk DRAW_STROKE 0.5)


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
    

squareTip :: ArrowTip
squareTip = 
    ArrowTip
      { retract_distance = const 1
      , tip_half_len     = 0.5
      , tip_deco         = filledTipPath squareSpec
      }

osquareTip :: ArrowTip
osquareTip = 
    ArrowTip
      { retract_distance = const 1
      , tip_half_len     = 0.5
      , tip_deco         = closedTipPath squareSpec
      }



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


diamondTip :: ArrowTip
diamondTip = 
    ArrowTip
      { retract_distance = const 1
      , tip_half_len     = 0.5
      , tip_deco         = filledTipPath (diamondSpec 1)
      }

odiamondTip :: ArrowTip
odiamondTip = 
    ArrowTip
      { retract_distance = const 1
      , tip_half_len     = 0.5
      , tip_deco         = closedTipPath (diamondSpec 1)
      }


diamondWideTip :: ArrowTip
diamondWideTip = 
    ArrowTip
      { retract_distance = const 2
      , tip_half_len     = 1.0
      , tip_deco         = filledTipPath (diamondSpec 2)
      }

odiamondWideTip :: ArrowTip
odiamondWideTip = 
    ArrowTip
      { retract_distance = const 2
      , tip_half_len     = 1.0
      , tip_deco         = closedTipPath (diamondSpec 2)
      }


curveTipTrail :: Radian -> AnaTrail En
curveTipTrail theta = 
    anaCatTrail dv $ vectrapCCW v1 <> vectrapCCW v2
  where
    dv  = orthoVec (-1.0)   0.75  theta     -- back and up
    v1  = orthoVec   1.0  (-0.75) theta     -- fwd and down
    v2  = orthoVec (-1.0) (-0.75) theta     -- back and down


vectrapCW :: (Real u, Floating u) => Vec2 u -> CatTrail u
vectrapCW v1 = trapcurve CW w h quarter_pi ang
  where
    w   = vlength v1
    h   = w / 4
    ang = vdirection v1


vectrapCCW :: (Real u, Floating u) => Vec2 u -> CatTrail u
vectrapCCW v1 = trapcurve CCW w h quarter_pi ang
  where
    w   = vlength v1
    h   = w / 4
    ang = vdirection v1


curveTip :: ArrowTip
curveTip = 
    ArrowTip
      { retract_distance = const 0
      , tip_half_len     = 0.5
      , tip_deco         = body
      }
  where
    body = promoteLocTheta $ \pt theta -> 
             localize (join_bevel . solid_stroke_tip) $ 
               supplyLoc pt $ drawAnaTrail OSTROKE $ curveTipTrail theta



curveTipRevTrail :: Radian -> AnaTrail En
curveTipRevTrail theta = 
    anaCatTrail dv $ vectrapCW v1 <> vectrapCW v2
  where
    dv  = orthoVec   0.0    0.75  theta     -- just up
    v1  = orthoVec (-1.0) (-0.75) theta     -- back and down
    v2  = orthoVec   1.0  (-0.75) theta     -- fwd and down



revcurveTip :: ArrowTip
revcurveTip = 
    ArrowTip
      { retract_distance = const 1
      , tip_half_len     = 0.5
      , tip_deco         = body
      }
  where
    body = promoteLocTheta $ \pt theta -> 
             localize (join_bevel . solid_stroke_tip) $ 
               supplyLoc pt $ drawAnaTrail OSTROKE $ curveTipRevTrail theta


