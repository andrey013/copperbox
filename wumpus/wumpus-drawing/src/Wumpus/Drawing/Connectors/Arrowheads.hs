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
   
    ArrowTip 
    
  , leftRightArrow      -- temporarily here

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


import Wumpus.Drawing.Connectors.ConnectorPaths ( Connector )
import Wumpus.Drawing.Paths.Absolute

import Wumpus.Basic.Kernel                      -- package: wumpus-basic

import Wumpus.Core                              -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace

import Control.Applicative

type ArrowConnector u = ConnectorImage u (AbsPath u)

type TipDraw = Point2 En -> Radian -> GraphicAns En


data ArrowTip u = ArrowTip { getArrowTip :: CF (u, TipDraw) }


-- | Connector with two arrow tips, possibly different.
--
leftRightArrow :: (Real u, Floating u, InterpretUnit u) 
               => ArrowTip u -> ArrowTip u -> Connector u 
               -> ArrowConnector u
leftRightArrow tipl tipr conn = promoteR2 $ \p0 p1 ->
    apply2R2 conn p0 p1 >>= \full_path -> 
    getArrowTip tipl    >>= \(dl,mkl) -> 
    getArrowTip tipr    >>= \(dr,mkr) -> 
    uconvertCtxF p0     >>= \emp0     ->       
    uconvertCtxF p1     >>= \emp1     ->       
    let angl            = tipDirectionL dl full_path
        angr            = tipDirectionR dr full_path
        short_path      = shortenPath dl dr full_path
        deco            = convertTipAns $ mkl emp0 angl `oplus`  mkr emp1 angr  
    in fmap (replaceAns full_path) $ 
         decorateR0 deco $ toPrimPath short_path >>= openStroke


convertTipAns :: InterpretUnit u => GraphicAns En -> Graphic u
convertTipAns = uconvImageF . pure 

-- | Helper - direction looks best at half the retract distance.
--
tipDirectionL :: (Real u, Floating u) => u -> AbsPath u -> Radian
tipDirectionL u absp | u <= 0   = directionL absp
                     |otherwise = directionL $ shortenL (0.5*u) absp
   
tipDirectionR :: (Real u, Floating u) => u -> AbsPath u -> Radian
tipDirectionR u absp | u <= 0   = directionR absp
                     |otherwise = directionR $ shortenR (0.5*u) absp
   


makeArrowTip :: Query u -> LocThetaGraphic En -> ArrowTip u
makeArrowTip mq gf = ArrowTip body
  where
    body = drawingCtx >>= \ctx -> 
           let rdist = runCF ctx mq
               drawf = runCF ctx gf
           in return (rdist,drawf)



-- | Arrow tips are drawn with a sloid line even if the connector
-- line is dashed (tips also override round corners)

solid_stroke_tip :: DrawingContextF
solid_stroke_tip = reset_drawing_metrics


type PointGen = Radian -> [Vec2 En]


filledTipPath :: PointGen -> LocThetaGraphic En
filledTipPath fn = 
    localize fill_use_stroke_colour $ promoteR2 $ \pt theta ->
      let vs = fn theta in vertexPP (map (pt .+^) vs) >>= filledPath


closedTipPath :: PointGen -> LocThetaGraphic En
closedTipPath fn = 
    localize solid_stroke_tip $ promoteR2 $ \pt theta ->
      let vs = fn theta in vertexPP (map (pt .+^) vs) >>= closedStroke


openTipPath :: PointGen -> LocThetaGraphic En
openTipPath fn = 
    localize solid_stroke_tip $ promoteR2 $ \pt theta ->
      let vs = fn theta in vertexPP (map (pt .+^) vs) >>= openStroke



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



filledTri :: InterpretUnit u => Radian -> ArrowTip u
filledTri ang = 
    makeArrowTip (uconvertCtx1 (1::En)) (filledTipPath spec)
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
    makeArrowTip (uconvertCtx1 (1::En)) (closedTipPath spec)
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
    makeArrowTip (uconvertCtx1 (1::En)) (filledTipPath spec)
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
    makeArrowTip (uconvertCtx1 (1::En)) (closedTipPath spec)
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
    makeArrowTip (pure 0) (openTipPath spec)
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
    makeArrowTip (uconvertCtx1 (1::En)) (openTipPath spec)
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
    makeArrowTip (pure 0) (openTipPath spec)
  where
    spec theta = let oa = avec (theta + ang90) 0.5
                     ob = avec (theta - ang90) 0.5
                 in [oa, ob]


bracket :: InterpretUnit u => ArrowTip u
bracket = 
    makeArrowTip (pure 0) (openTipPath spec)
  where
    spec theta = let oa = avec (theta + ang90) 0.5
                     ob = avec (theta - ang90) 0.5
                     dv = avec theta (-0.5)
                 in [ oa ^+^ dv, oa, ob, ob ^+^ dv ]



diskTip :: InterpretUnit u => ArrowTip u
diskTip = 
    makeArrowTip (uconvertCtx1 (1::En)) 
                 (promoteR2 $ \pt theta -> body theta `at` pt)
  where
    body :: Radian -> LocGraphic En
    body theta = let v1 = avec theta (-0.5)
                 in localize fill_use_stroke_colour $ 
                      moveStart (displaceVec v1) (filledDisk 0.5)


odiskTip :: InterpretUnit u => ArrowTip u
odiskTip = 
    makeArrowTip (uconvertCtx1 (1::En)) 
                 (promoteR2 $ \pt theta -> body theta `at` pt)
  where
    body :: Radian -> LocGraphic En
    body theta = let v1 = avec theta (-0.5)
                 in localize solid_stroke_tip $ 
                      moveStart (displaceVec v1) (strokedDisk 0.5)


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
    makeArrowTip (uconvertCtx1 (1::En)) (filledTipPath squareSpec)

osquareTip :: InterpretUnit u => ArrowTip u
osquareTip = 
    makeArrowTip (uconvertCtx1 (1::En)) (closedTipPath squareSpec)



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
    makeArrowTip (uconvertCtx1 (1::En)) (filledTipPath $ diamondSpec 1)

odiamondTip :: InterpretUnit u => ArrowTip u
odiamondTip = 
    makeArrowTip (uconvertCtx1 (1::En)) (closedTipPath $ diamondSpec 1)


diamondWideTip :: InterpretUnit u => ArrowTip u
diamondWideTip = 
    makeArrowTip (uconvertCtx1 (2::En)) (filledTipPath $ diamondSpec 2)

odiamondWideTip :: InterpretUnit u => ArrowTip u
odiamondWideTip = 
    makeArrowTip (uconvertCtx1 (2::En)) (closedTipPath $ diamondSpec 2)


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
    makeArrowTip (pure 0) 
                 (promoteR2 $ \pt theta -> 
                    localize (join_bevel . solid_stroke_tip) $ 
                      toPrimPath (curveTipPath pt theta) >>= openStroke)



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
    makeArrowTip (uconvertCtx1 (1::En)) 
                 (promoteR2 $ \pt theta -> 
                    localize (join_bevel . solid_stroke_tip) $ 
                      toPrimPath (curveTipRevPath pt theta) >>= openStroke)


    
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