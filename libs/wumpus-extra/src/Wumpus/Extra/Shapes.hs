{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Extra.Shapes
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC with TypeFamilies and more
--
-- Basic graphics objects (lines segments, curves, polygons...)
-- 
--------------------------------------------------------------------------------

module Wumpus.Extra.Shapes
  ( 

  -- * Anchors
    AnchorCenter(..)
  , AnchorCardinal(..)

  -- * Coordinate (drawable point)
  , Coordinate

  , coordinate
  , drawCoordinate

  -- * Rectangle
  , Rectangle
  , rectangle
  , strokeRectangle

  -- * TextLine
  , Textline
  , textline

  ) where

import Wumpus.Core hiding ( center, CTM )
import Wumpus.Extra.Utils

import Data.AffineSpace         -- package: vector-space
import Data.VectorSpace


--------------------------------------------------------------------------------
-- Shapes will generally include a translation matrix...

type CTM u = Matrix3'3 u

translateCTM :: Num u => u -> u -> CTM u -> CTM u
translateCTM x y m = translationMatrix x y * m

scaleCTM :: Num u => u -> u -> CTM u -> CTM u
scaleCTM x y m = scalingMatrix x y * m

rotateCTM       :: (Floating u, Real u) => Radian -> CTM u -> CTM u
rotateCTM r m   = rotationMatrix r * m

rotateAboutCTM  :: (Floating u, Real u) => Radian -> Point2 u -> CTM u -> CTM u
rotateAboutCTM r pt m = originatedRotationMatrix r pt * m





--------------------------------------------------------------------------------
-- Anchors

class AnchorCenter a where
  center :: DUnit a ~ u => a -> Point2 u

class AnchorCardinal a where
  north :: DUnit a ~ u => a -> Point2 u
  south :: DUnit a ~ u => a -> Point2 u
  east  :: DUnit a ~ u => a -> Point2 u
  west  :: DUnit a ~ u => a -> Point2 u

  northeast :: DUnit a ~ u => a -> Point2 u
  southeast :: DUnit a ~ u => a -> Point2 u
  southwest :: DUnit a ~ u => a -> Point2 u
  northwest :: DUnit a ~ u => a -> Point2 u

--------------------------------------------------------------------------------

-- | Coordinates

data Coordinate u = Coordinate
      { coord_center          :: Point2 u
      , coord_ctm             :: CTM u 
      }

type instance DUnit (Coordinate u) = u

-- helper - extract coord_center w.r.t. the CTM 
--
inCtxCoord :: Num u => Coordinate u -> (Point2 u -> a) -> a
inCtxCoord coord f = f $ ctm *# pt
    where
      ctm       = coord_ctm    coord
      pt        = coord_center coord

 

-- Instances 

instance Num u => AnchorCenter (Coordinate u) where
  center c = inCtxCoord c id


instance (Floating u, Real u) => Rotate (Coordinate u) where
  rotate r = pstar (\m s -> s { coord_ctm = rotateCTM r m }) coord_ctm 

instance (Floating u, Real u) => RotateAbout (Coordinate u) where
  rotateAbout r pt = 
      pstar (\m s -> s { coord_ctm = rotateAboutCTM r pt m }) coord_ctm

instance Num u => Scale (Coordinate u) where
  scale x y = pstar (\m s -> s { coord_ctm = scaleCTM x y m }) coord_ctm

instance Num u => Translate (Coordinate u) where
  translate x y = 
     pstar (\m s -> s { coord_ctm = translateCTM x y m }) coord_ctm


--

coordinate :: Num u => Point2 u -> Coordinate u
coordinate pt = Coordinate pt identityMatrix

drawCoordinate :: (Fractional u, Ellipse t) => t -> Coordinate u -> Primitive u
drawCoordinate t coord = ellipse t 2 2 (inCtxCoord coord id)

--------------------------------------------------------------------------------

-- | Rectangles.
--
data Rectangle u = Rectangle 
      { rect_bottom_left      :: Point2 u
      , rect_upper_right      :: Point2 u
      , rect_ctm              :: CTM u
      }

type instance DUnit (Rectangle u) = u


inCtxRect :: Num u
          => Rectangle u -> (CTM u -> Point2 u -> Vec2 u -> a) -> a
inCtxRect rtangle f = f ctm bl (tr .-. bl)
    where
      ctm       = rect_ctm rtangle
      bl        = rect_bottom_left rtangle
      tr        = rect_upper_right rtangle

-- Instances 
  

instance (Fractional u) => AnchorCenter (Rectangle u) where
  center rtangle = inCtxRect rtangle $ \ ctm bl v -> 
      let v' = v ^* 0.5 in (ctm *#) $ (bl .+^ v')



instance (Fractional u) =>  AnchorCardinal (Rectangle u) where
  north rtangle = inCtxRect rtangle $ \ ctm bl (V2 w h) -> 
      (ctm *#) $ bl .+^ (V2 (w*0.5) h)

  south rtangle = inCtxRect rtangle $ \ ctm bl (V2 w _) -> 
      (ctm *#) $ bl .+^ (V2 (w*0.5) 0)

  east rtangle = inCtxRect rtangle $ \ ctm bl (V2 w h) -> 
      (ctm *#) $ bl .+^ (V2 w (h*0.5))

  west rtangle = inCtxRect rtangle $ \ ctm bl (V2 _ h) -> 
      (ctm *#) $ bl .+^ (V2 0 (h*0.5))

  northeast rtangle = inCtxRect rtangle $ \ ctm bl v -> 
      (ctm *#) $ bl .+^ v

  southeast rtangle = inCtxRect rtangle $ \ ctm bl (V2 w _) -> 
      (ctm *#) $ bl .+^ (V2 w 0)

  southwest rtangle = inCtxRect rtangle $ \ ctm bl _ -> ctm *# bl

  northwest rtangle = inCtxRect rtangle $ \ ctm bl (V2 _ h) -> 
      (ctm *#) $ bl .+^ (V2 0 h)


instance (Floating u, Real u) => Rotate (Rectangle u) where
  rotate r = pstar (\m s -> s { rect_ctm = rotateCTM r m }) rect_ctm 

instance (Floating u, Real u) => RotateAbout (Rectangle u) where
  rotateAbout r pt = 
      pstar (\m s -> s { rect_ctm = rotateAboutCTM r pt m }) rect_ctm

instance Num u => Scale (Rectangle u) where
  scale x y = pstar (\m s -> s { rect_ctm = scaleCTM x y m }) rect_ctm

instance Num u => Translate (Rectangle u) where
  translate x y = 
     pstar (\m s -> s { rect_ctm = translateCTM x y m }) rect_ctm


-- | @rectangle : width * height * center_pt -> rectangle@
--
rectangle :: Fractional u => u -> u -> Point2 u -> Rectangle u
rectangle w h ctr = Rectangle (ctr .-^ v) (ctr .+^ v) identityMatrix
  where
    v = V2 (w * 0.5) (h * 0.5) 


-- | make Picture or Primitive?
--
strokeRectangle :: (Fractional u, Ord u, Stroke t) 
                => t -> Rectangle u -> Primitive u
strokeRectangle t rtangle = cstroke t $ vertexPath [bl,br,tr,tl]
  where
    bl        = southwest rtangle
    tr        = northeast rtangle
    br        = southeast rtangle
    tl        = northwest rtangle

--------------------------------------------------------------------------------
-- Text label

data Textline u = Textline
      { text_string           :: String
      , text_attr             :: FontAttr
      , text_rect             :: Rectangle u
      }

type instance DUnit (Textline u) = u
      

instance (Fractional u) => AnchorCenter (Textline u) where
  center = center . text_rect 


instance (Floating u, Real u) => Rotate (Textline u) where
  rotate r = pstar (\m s -> s { text_rect = rotate r m }) text_rect


instance (Floating u, Real u) => RotateAbout (Textline u) where
  rotateAbout r pt = 
      pstar (\m s -> s { text_rect = rotateAbout r pt m }) text_rect

instance Num u => Scale (Textline u) where
  scale x y = pstar (\m s -> s { text_rect = scale x y m }) text_rect

instance Num u => Translate (Textline u) where
  translate x y = 
     pstar (\m s -> s { text_rect = translate x y m }) text_rect

textline :: (Fractional u, Ord u) 
         => FontAttr -> String -> Point2 u -> Textline u
textline attr s ctr = Textline s attr $ rectangle w h ctr
  where 
    (V2 w h)           = tr .-. bl
    (bl, _br, tr, _tl) = corners $ boundary $ textlabel attr s ctr


-- Can\'t apply affine transformations to Primitives.
-- This means drawing a TextLine has to make a Picture,
-- and this is a bit disappointing as other Shapes can be 
-- accommodated as Primitives.
--
-- (Other shapes have their constituent points transformed
-- before they are draw with wumpus-core).
--
-- Having e.g. many coordinates rendering as individual 
-- Pictures is inefficient.
-- 

{-
drawTextline :: TextLine u -> Picture u
drawTextline 
-}