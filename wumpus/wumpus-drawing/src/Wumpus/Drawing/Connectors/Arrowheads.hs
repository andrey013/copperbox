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


type PointGen = Radian -> [Vec2 En]


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




ang90 :: Radian
ang90 = pi / 2

ang60 :: Radian
ang60 = pi / 3

ang45 :: Radian
ang45 = pi / 4




filledTri :: Radian -> ArrowTip
filledTri ang = 
    ArrowTip
      { retract_distance = const 1
      , tip_half_len     = 0.5
      , tip_deco         = filledTipPath spec
      }
  where
    spec theta = let (v1,v2) = tripointsFromTip 1 ang theta 
                 in [zeroVec, v1, v2]

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
      , tip_deco         = closedTipPath spec
      }
  where
    spec theta = let (v1,v2) = tripointsFromTip 1 ang theta 
                 in [zeroVec, v1, v2]


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
      , tip_deco         = filledTipPath spec
      }
  where
    spec theta = let (v0,v1,v2) = revTripointsFromTip 1 ang theta 
                 in [v0, v1, v2]

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
      , tip_deco         = closedTipPath spec
      }
  where
    spec theta = let (v0,v1,v2) = revTripointsFromTip 1 ang theta 
                 in [v0, v1, v2]


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
      , tip_deco         = openTipPath spec
      }
  where
    spec theta = let (v1,v2) = tripointsFromTip 1 ang theta 
                 in [v1,zeroVec,v2]


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
      , tip_deco         = openTipPath spec
      }
  where
    spec theta = let (v0,v1,v2) = revTripointsFromTip 1 ang theta 
                 in [v1,v0,v2]

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


curveTipTrail :: Radian -> PlacedTrail En
curveTipTrail theta = 
    placeCatTrail dv $ vectrapCCW v1 <> vectrapCCW v2
  where
    dv  = orthoVec (-1.0)   0.75  theta     -- back and up
    v1  = orthoVec   1.0  (-0.75) theta     -- fwd and down
    v2  = orthoVec (-1.0) (-0.75) theta     -- back and down


vectrapCW :: (Real u, Floating u) => Vec2 u -> CatTrail u
vectrapCW v1 = trapcurveCW w h ang45 ang
  where
    w   = vlength v1
    h   = w / 4
    ang = vdirection v1


vectrapCCW :: (Real u, Floating u) => Vec2 u -> CatTrail u
vectrapCCW v1 = trapcurveCCW w h quarter_pi ang
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
               supplyLoc pt $ drawPlacedTrail OSTROKE $ curveTipTrail theta



curveTipRevTrail :: Radian -> PlacedTrail En
curveTipRevTrail theta = 
    placeCatTrail dv $ vectrapCW v1 <> vectrapCW v2
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
               supplyLoc pt $ drawPlacedTrail OSTROKE $ curveTipRevTrail theta


