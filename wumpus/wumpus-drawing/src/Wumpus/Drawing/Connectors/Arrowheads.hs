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
import Wumpus.Drawing.Basis.InclineTrails
import Wumpus.Drawing.Connectors.Base

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

-- import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace

import Data.Monoid


-- | Arrow tips are drawn with a solid line even if the connector
-- line is dashed (tips also override round corners)

solid_stroke_tip :: DrawingContextF
solid_stroke_tip = solid_line -- reset_drawing_metrics



type TrailGen = Radian -> AnaTrail En


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
closedTriTrail ang = \theta ->
    modifyAna (\v1 -> v1 ^+^ theta_left 1 theta) $
      incline_triangle ang (avec theta 1)



filledTri :: Radian -> ArrowTip
filledTri ang = 
    ArrowTip
      { retract_distance = 1
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
      { retract_distance = 1
      , tip_half_len     = 0.5
      , tip_deco         = closedTrailTip $ closedTriTrail ang
      }


otri90 :: ArrowTip
otri90 = strokedClosedTri ang90

otri60 :: ArrowTip
otri60 = strokedClosedTri ang60

otri45 :: ArrowTip
otri45 = strokedClosedTri ang45


-- | All three lines are stated.
--
revClosedTriSpec :: Radian -> TrailGen
revClosedTriSpec ang = \theta -> 
{-    modifyAna (\v1 -> v1 ^+^ theta_left 1 theta) $ -}
      incline_triangle ang (avec theta (-1))


filledRevTri :: Radian -> ArrowTip
filledRevTri ang = 
    ArrowTip
      { retract_distance = 0.5
      , tip_half_len     = 0.5
      , tip_deco         = fillTrailTip $ revClosedTriSpec ang
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
      { retract_distance = 1
      , tip_half_len     = 0.5
      , tip_deco         = closedTrailTip $ revClosedTriSpec ang
      }

orevtri90 :: ArrowTip
orevtri90 = strokedClosedRevTri ang90

orevtri60 :: ArrowTip
orevtri60 = strokedClosedRevTri ang60

orevtri45 :: ArrowTip
orevtri45 = strokedClosedRevTri ang45


barbSpec :: Radian -> TrailGen
barbSpec ang = \theta -> 
    modifyAna (\v1 -> v1 ^+^ avec theta (-1)) $ incline_barb ang (avec theta 1)


strokedBarb :: Radian -> ArrowTip
strokedBarb ang = 
    ArrowTip
      { retract_distance = 0
      , tip_half_len     = 0.5
      , tip_deco         = openTrailTip $ barbSpec ang
      }

barb90 :: ArrowTip
barb90 = strokedBarb ang90

barb60 :: ArrowTip
barb60 = strokedBarb ang60

barb45 :: ArrowTip
barb45 = strokedBarb ang45


revBarbSpec :: Radian -> TrailGen
revBarbSpec ang = \theta -> incline_barb ang (avec theta (-1))

strokedRevBarb :: Radian -> ArrowTip
strokedRevBarb ang = 
    ArrowTip
      { retract_distance = 1
      , tip_half_len     = 0.5
      , tip_deco         = openTrailTip $ revBarbSpec ang 
      }


revbarb90 :: ArrowTip
revbarb90 = strokedRevBarb ang90

revbarb60 :: ArrowTip
revbarb60 = strokedRevBarb ang60

revbarb45 :: ArrowTip
revbarb45 = strokedRevBarb ang45


perpSpec :: TrailGen
perpSpec ang = 
    anaCatTrail (theta_up 0.5 ang) $ trail_theta_down 1 ang

perp :: ArrowTip
perp = 
    ArrowTip
      { retract_distance = 0
      , tip_half_len     = 0
      , tip_deco         = openTrailTip perpSpec
      }


bracketSpec :: TrailGen
bracketSpec ang = anaCatTrail (orthoVec (-0.5) 0.5 ang) catt
  where
    catt = mconcat [ trail_theta_right 0.5 ang
                   , trail_theta_down  1.0 ang
                   , trail_theta_left  0.5 ang 
                   ]


bracket :: ArrowTip
bracket = 
    ArrowTip
      { retract_distance = 0.0
      , tip_half_len     = 0.5
      , tip_deco         = openTrailTip bracketSpec
      }

diskBody :: DrawMode -> Radian -> LocGraphic En
diskBody mode theta = 
    localize fill_use_stroke_colour $ moveStart vback $ dcDisk mode 0.5
  where
    vback = theta_left 0.5 theta

diskTip :: ArrowTip
diskTip = 
    ArrowTip
      { retract_distance = 0.5
      , tip_half_len     = 0.5
      , tip_deco         = promoteLocTheta $ \pt theta -> 
                             diskBody DRAW_FILL theta `at` pt
      }


odiskTip :: ArrowTip
odiskTip = 
    ArrowTip
      { retract_distance = 1
      , tip_half_len     = 0.5
      , tip_deco         = promoteLocTheta $ \pt theta -> 
                             diskBody DRAW_STROKE theta `at` pt
      }


-- | Note - need to draw square East-West rather than West-East
-- hence the base_width is negative.
--
squareSpec :: TrailGen
squareSpec theta = incline_square $ avec theta (-1)


squareTip :: ArrowTip
squareTip = 
    ArrowTip
      { retract_distance = 1
      , tip_half_len     = 0.5
      , tip_deco         = fillTrailTip squareSpec
      }
    

osquareTip :: ArrowTip
osquareTip = 
    ArrowTip
      { retract_distance = 1
      , tip_half_len     = 0.5
      , tip_deco         = closedTrailTip squareSpec
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
diamondSpec :: En -> TrailGen
diamondSpec w theta = incline_diamond 1 $ avec theta (-w)

diamondTip :: ArrowTip
diamondTip = 
    ArrowTip
      { retract_distance = 0.5
      , tip_half_len     = 0.5
      , tip_deco         = fillTrailTip $ diamondSpec 1
      }

odiamondTip :: ArrowTip
odiamondTip = 
    ArrowTip
      { retract_distance = 1
      , tip_half_len     = 0.5
      , tip_deco         = closedTrailTip $ diamondSpec 1
      }


diamondWideTip :: ArrowTip
diamondWideTip = 
    ArrowTip
      { retract_distance = 1.0
      , tip_half_len     = 1.0
      , tip_deco         = fillTrailTip $ diamondSpec 2
      }

odiamondWideTip :: ArrowTip
odiamondWideTip = 
    ArrowTip
      { retract_distance = 2.0
      , tip_half_len     = 1.0
      , tip_deco         = closedTrailTip $ diamondSpec 2
      }


curveTipSpec :: TrailGen
curveTipSpec theta = 
    anaCatTrail dv $ vectrapCCW v1 <> vectrapCCW v2
  where
    dv  = orthoVec (-1.0)   0.75  theta     -- back and up
    v1  = orthoVec   1.0  (-0.75) theta     -- fwd and down
    v2  = orthoVec (-1.0) (-0.75) theta     -- back and down


vectrapCW :: (Real u, Floating u) => Vec2 u -> CatTrail u
vectrapCW v1 = trapCurve CW w h quarter_pi ang
  where
    w   = vlength v1
    h   = w / 4
    ang = vdirection v1


vectrapCCW :: (Real u, Floating u) => Vec2 u -> CatTrail u
vectrapCCW v1 = trapCurve CCW w h quarter_pi ang
  where
    w   = vlength v1
    h   = w / 4
    ang = vdirection v1


curveTip :: ArrowTip
curveTip = 
    ArrowTip
      { retract_distance = 0
      , tip_half_len     = 0.5
      , tip_deco         = body
      }
  where
    body = localize (cap_round . join_round . solid_stroke_tip) $ 
             openTrailTip curveTipSpec



curveTipRevSpec :: TrailGen
curveTipRevSpec theta = 
    anaCatTrail dv $ vectrapCW v1 <> vectrapCW v2
  where
    dv  = orthoVec   0.0    0.75  theta     -- just up
    v1  = orthoVec (-1.0) (-0.75) theta     -- back and down
    v2  = orthoVec   1.0  (-0.75) theta     -- fwd and down



revcurveTip :: ArrowTip
revcurveTip = 
    ArrowTip
      { retract_distance = 1
      , tip_half_len     = 0.5
      , tip_deco         = body
      }
  where
    body = localize (cap_round . join_round . solid_stroke_tip) $ 
             openTrailTip curveTipRevSpec


