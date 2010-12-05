{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Basic.Kernel.Objects.LocGraphic
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  highly unstable
-- Portability :  GHC 
--
-- Extended Graphic object - the origin (start point) is 
-- implicitly supplied as well as the DrawingContext.
--
--------------------------------------------------------------------------------

module Wumpus.Basic.Kernel.Objects.LocGraphic
  (

  -- * LocGraphic  
    LocGraphic
  , DLocGraphic


  -- * Run functions
  , runLocGraphic

  , emptyLocGraphic

  , supplyPt

  , textline
  , centermonoTextline
  , escapedline

  , hkernline
  , vkernline



  , straightLine

  , strokedRectangle
  , filledRectangle
  , borderedRectangle

  , strokedCircle
  , filledCircle
  , borderedCircle

  , strokedEllipse
  , filledEllipse  
  , borderedEllipse
  
  , strokedDisk
  , filledDisk
  , borderedDisk



  ) where

import Wumpus.Basic.Kernel.Base.BaseDefs
import Wumpus.Basic.Kernel.Base.ContextFun
import Wumpus.Basic.Kernel.Base.DrawingContext
import Wumpus.Basic.Kernel.Base.QueryDC
import Wumpus.Basic.Kernel.Base.WrappedPrimitive
import Wumpus.Basic.Kernel.Geometry.Paths
import Wumpus.Basic.Kernel.Objects.Graphic

import Wumpus.Core                      -- package: wumpus-core

import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace


--------------------------------------------------------------------------------
-- LocGraphic

-- | Commonly graphics take a start point as well as a drawing 
-- context.
-- 
-- Here they are called a LocGraphic - graphic with a (starting) 
-- location.
--
type LocGraphic u       = LocCF u (PrimGraphic u)

type DLocGraphic        = LocGraphic Double






--------------------------------------------------------------------------------
-- Run functions


runLocGraphic :: DrawingContext -> Point2 u -> LocGraphic u -> PrimGraphic u
runLocGraphic ctx pt df = runCF ctx (unCF1 pt df)




-------------------------------------------------------------------------------
-- Dropping /answers/



-- extrLocGraphic :: LocImage u a -> LocGraphic u
-- extrLocGraphic = postpro1 snd 



--------------------------------------------------------------------------------

locPrimGraphic :: (Point2 u -> Primitive u) -> (Point2 u -> PrimGraphic u)
locPrimGraphic fn = primGraphic . fn




emptyLocGraphic :: Num u => LocGraphic u
emptyLocGraphic =
    withStrokeAttr $ \rgb attr -> locPrimGraphic (ostroke rgb attr . emptyPath)




textline :: Num u => String -> LocGraphic u
textline ss =
    withTextAttr $ \rgb attr -> locPrimGraphic (textlabel rgb attr ss)



-- | As 'textline' but the supplied point is the /center/.
--
-- Centered is inexact - it is calculated with monospaced font
-- metrics.
-- 
centermonoTextline :: (Fractional u, Ord u, FromPtSize u) 
                   => String -> LocGraphic u
centermonoTextline ss = monoVecToCenter ss >>= \v ->
                          localPoint (vecdisplace (negateV v)) (textline ss)



escapedline :: Num u => EscapedText -> LocGraphic u
escapedline ss =
    withTextAttr $ \rgb attr -> locPrimGraphic (escapedlabel rgb attr ss)

{-
rescapedline :: Num u => EscapedText -> LocThetaGraphic u
rescapedline ss = 
    withTextAttr $ \rgb attr -> thetaLocPrimGraphic 
                                  (\pt ang -> rescapedlabel rgb attr ss pt ang)

-}

{-

-- built on LocImage not loc Graphic...

-- | Point is the baseline left of the bottom line, text is 
-- left-aligned.
--
textlineMulti :: Fractional u => [String] -> LocGraphic u
textlineMulti xs = baselineSpacing >>= \dy -> 
    extrLocGraphic $ go (tmStep dy) xs
  where
    -- go /starts/ at the end of the list and works back.
    go fn []      = fn ""       -- not ideal, better than error
    go fn [s]     = fn s
    go fn (s:ss)  = let ans = go fn ss in ans `feedPt` fn s

-- LocImage u (Point2 u) deserved to be a new type synonym
-- as it models PostScript\'s @show@ 


tmStep :: Num u => u -> String -> LocImage u (Point2 u) 
tmStep dy str = intoLocImage (pure $ \pt -> pt .+^ vvec dy) (textline str)

-}

hkernline :: Num u => [KerningChar u] -> LocGraphic u
hkernline ks = 
    withTextAttr $ \rgb attr -> locPrimGraphic (hkernlabel rgb attr ks)
      

vkernline :: Num u => [KerningChar u] -> LocGraphic u
vkernline ks = 
    withTextAttr $ \rgb attr -> locPrimGraphic (vkernlabel rgb attr ks)
  



--------------------------------------------------------------------------------


strokedEllipse :: Num u => u -> u -> LocGraphic u
strokedEllipse hw hh =  
    withStrokeAttr $ \rgb attr -> locPrimGraphic (strokeEllipse rgb attr hw hh)
   

filledEllipse :: Num u => u -> u -> LocGraphic u
filledEllipse hw hh =  
    withFillAttr $ \rgb -> locPrimGraphic (fillEllipse rgb hw hh)
  

borderedEllipse :: Num u => u -> u -> LocGraphic u
borderedEllipse hw hh = 
    withBorderedAttr $ \frgb attr srgb -> 
      locPrimGraphic (fillStrokeEllipse frgb attr srgb hw hh)


-- | Supplying a point to a 'CFGraphic' takes it to a regular 
-- 'Graphic'.
--
supplyPt :: Point2 u -> LocGraphic u -> Graphic u
supplyPt pt gf = fmap ($ pt) gf 


-- localPoint :: (Point2 u -> Point2 u) -> LocGraphic u -> LocGraphic u
-- localPoint = moveLoc


straightLine :: Fractional u => Vec2 u -> LocGraphic u
straightLine v = 
    promote1 $ \pt -> openStroke $ primPath pt [lineTo $ pt .+^ v]


-- This is basically the cardinal-prime combinator with arguments 
-- at specific types 
-- 
-- > cardinal'  :: (a -> r1 -> ans) -> (r2 -> a) -> (r1 -> r2 -> ans)
--

drawWith :: (PrimPath u -> Graphic u) -> (Point2 u -> PrimPath u) -> LocGraphic u 
drawWith mf g = promote1 $ \pt -> (mf $ g pt)

-- | Supplied point is /bottom left/.
--
strokedRectangle :: Fractional u => u -> u -> LocGraphic u
strokedRectangle w h = drawWith closedStroke (rectanglePath w h)


-- | Supplied point is /bottom left/.
--
filledRectangle :: Fractional u => u -> u -> LocGraphic u
filledRectangle w h = drawWith borderedPath (rectanglePath w h) 

-- | Supplied point is /bottom left/.
--
borderedRectangle :: Fractional u => u -> u -> LocGraphic u
borderedRectangle w h = drawWith borderedPath (rectanglePath w h) 



-- | Supplied point is center. Circle is drawn with Bezier 
-- curves. 
--
strokedCircle :: Floating u => Int -> u -> LocGraphic u
strokedCircle n r = drawWith closedStroke (curvedPath . bezierCircle n r)



-- | Supplied point is center. Circle is drawn with Bezier 
-- curves. 
--
filledCircle :: Floating u => Int -> u -> LocGraphic u
filledCircle n r = drawWith filledPath (curvedPath . bezierCircle n r)



-- | Supplied point is center. Circle is drawn with Bezier 
-- curves. 
--
borderedCircle :: Floating u => Int -> u -> LocGraphic u
borderedCircle n r = drawWith borderedPath (curvedPath . bezierCircle n r)


-- | 'disk' is drawn with Wumpus-Core\'s @ellipse@ primitive.
--
-- This is a efficient representation of circles using 
-- PostScript\'s @arc@ or SVG\'s @circle@ in the generated 
-- output. However, stroked-circles do not draw well after 
-- non-uniform scaling - the line width is scaled as well as 
-- the shape.
--
-- For stroked circles that can be scaled, consider making the 
-- circle from Bezier curves.
--
strokedDisk :: Num u => u -> LocGraphic u
strokedDisk radius = strokedEllipse radius radius


filledDisk :: Num u => u -> LocGraphic u
filledDisk radius = filledEllipse radius radius

borderedDisk :: Num u => u -> LocGraphic u
borderedDisk radius = borderedEllipse radius radius
