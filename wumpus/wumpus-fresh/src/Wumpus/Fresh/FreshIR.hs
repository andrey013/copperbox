{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Fresh.FreshIR
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Fresh picture.
--
--------------------------------------------------------------------------------


module Wumpus.Fresh.FreshIR
  ( 

    RGB255(..)
  , Picture(..)
  , DPicture

  , Primitive(..)
  , DPrimitive

  , PrimEllipse(..)
  , PrimCTM(..)

  , printPicture
  , frameMulti

  , identityCTM

  , ellipse_

  ) where

import Wumpus.Fresh.BoundingBox
import Wumpus.Fresh.FormatCombinators
import Wumpus.Fresh.Geometry
import Wumpus.Fresh.GraphicsState
import Wumpus.Fresh.OneList
import Wumpus.Fresh.Utils


import Data.AffineSpace                         -- package: vector-space
import Data.Semigroup                           -- package: algebra

import qualified Data.Foldable                  as F
import Data.Word

-- | Colours levels are in the range [0..255]
-- 
-- Note - this is the format used by SVG, whereas PostScript uses 
-- [0..1]. 
--
-- It is more efficient to prefer SVG here.
--
data RGB255 = RGB255 !Word8 !Word8 !Word8
  deriving (Eq,Ord,Show)

data Picture u = Leaf (Locale u) (OneList (Primitive u))
  deriving (Eq,Show)

type DPicture = Picture Double

data Primitive u = PEllipse EllipseProps (PrimEllipse u)
  deriving (Eq,Show)

type DPrimitive = Primitive Double


type Locale u = BoundingBox u


-- | Ellipses and circles are always closed.
--
data EllipseProps = EFill RGB255
                  | EStroke [StrokeAttr] RGB255 

                  -- Note - first colour fill, second colour stroke.
                  | EFillStroke RGB255 [StrokeAttr] RGB255 
  deriving (Eq,Show)


-- Ellipse represented by center and half_width * half_height
--
data PrimEllipse u = PrimEllipse 
      { ellipse_center        :: Point2 u
      , ellipse_half_width    :: u
      , ellipse_half_height   :: u 
      , ellipse_ctm           :: PrimCTM u
      } 
  deriving (Eq,Show)



-- Note - primitives are not considered to exist in an affine 
-- space. 
--
data PrimCTM u = PrimCTM 
      { ctm_scale_x     :: u
      , ctm_scale_y     :: u
      , ctm_rotation    :: Radian 
      }
  deriving (Eq,Show)


--------------------------------------------------------------------------------
-- family instances

type instance DUnit (Picture u)     = u
type instance DUnit (Primitive u)   = u
type instance DUnit (PrimEllipse u) = u

--------------------------------------------------------------------------------
-- instances

instance Format RGB255 where
  format (RGB255 0   0   0)    = text "*black*"
  format (RGB255 255 255 255)  = text "*white*"
  format (RGB255 r   g   b)    = integral r <> comma <> integral g 
                                            <> comma <> integral b

instance (Num u, PSUnit u) => Format (Picture u) where
  format (Leaf m prims)     = vcat [ text "** Leaf-pic **"
                                   , fmtLocale m 
                                   , indent 2 (fmtPrims prims) ]


fmtPrims :: PSUnit u => OneList (Primitive u) -> Doc
fmtPrims ones = snd $ F.foldl' fn (0,empty) ones
  where
    fn (n,acc) e = (n+1, vcat [ acc, text "-- leaf" <+> int n, format e, line])

fmtLocale :: (Num u, PSUnit u) => Locale u -> Doc
fmtLocale bb = format bb


instance PSUnit u => Format (Primitive u) where
  format (PEllipse props e)  = 
      vcat [text "ellipse:" <> format props, indent 2 (format e) ]


instance PSUnit u => Format (PrimEllipse u) where
  format (PrimEllipse ctr hw hh ctm) = text "center="   <> format ctr
                                   <+> text "hw="       <> dtruncFmt hw
                                   <+> text "hh="       <> dtruncFmt hh
                                   <+> text "ctm="      <> format ctm
  

instance PSUnit u => Format (PrimCTM u) where
  format (PrimCTM x y ang) = 
      parens (text "CTM" <+> text "sx="   <+> dtruncFmt x 
                         <+> text "sy="   <+> dtruncFmt y 
                         <+> text "ang="  <+> format ang)


instance Format EllipseProps where
  format (EFill rgb)          = format rgb <+> text "Fill"
  format (EStroke _ rgb)      = format rgb <+> text "Stroke"
  format (EFillStroke f _ s)  = format f <+> text "Fill" <> char '/'
                            <+> format s <+> text "Stroke"   


--------------------------------------------------------------------------------

instance (Real u, Floating u) => Boundary (Primitive u) where
  boundary (PEllipse _ e) = boundary e


instance (Real u, Floating u) => Boundary (PrimEllipse u) where
  boundary = ellipseBoundary









-- | Ellipse bbox is the bounding rectangle, rotated as necessary 
-- then retraced.
--
ellipseBoundary :: (Real u, Floating u) => PrimEllipse u -> BoundingBox u
ellipseBoundary (PrimEllipse pt hw0 hh0 (PrimCTM sx sy theta)) = 
    traceBoundary $ applyIf (theta /= 0) (map (rotm *#)) [ll,lr,ur,ul]
  where
    hw   = hw0 * sx
    hh   = hh0 * sy
    ll   = pt .+^ V2 (-hw) (-hh) 
    lr   = pt .+^ V2   hw  (-hh) 
    ur   = pt .+^ V2   hw    hh 
    ul   = pt .+^ V2 (-hw)   hh 
    rotm = rotationMatrix theta



--------------------------------------------------------------------------------

printPicture :: (Num u, PSUnit u) => Picture u -> IO ()
printPicture pic = putStrLn (show $ format pic) >> putStrLn []

-- This function throws an error when supplied the empty list.
--
frameMulti :: (Real u, Floating u) 
           => [Primitive u] -> Picture u
frameMulti []     = error "Wumpus.Core.Picture.frameMulti - empty list"
frameMulti (p:ps) = let (bb,ones) = step p ps 
                    in Leaf bb ones 
  where
    step a []     = (boundary a, one a)
    step a (x:xs) = let (bb',rest) = step x xs
                    in (boundary a `append` bb', cons a rest)



--------------------------------------------------------------------------------
-- Manipulating the Primitive CTM

identityCTM :: Num u => PrimCTM u
identityCTM = PrimCTM { ctm_scale_x = 1, ctm_scale_y = 1, ctm_rotation = 0 }





ellipse_ :: Num u => u -> u -> Point2 u -> Primitive u
ellipse_ hw hh pt = PEllipse (EFill (RGB255 127 0 0))body
  where
    body = PrimEllipse { ellipse_center        = pt
                       , ellipse_half_width    = hw
                       , ellipse_half_height   = hh
                       , ellipse_ctm           = identityCTM
                       } 