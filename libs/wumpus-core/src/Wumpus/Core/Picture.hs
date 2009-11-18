{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.Picture
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
--
--------------------------------------------------------------------------------

module Wumpus.Core.Picture 
  (
  
   -- * Construction
    blankPicture
  , frame
  , frameWithin
  , frameMulti
  , multi

  , vertexPath  
  , curvedPath

  -- * Constructing primitives
  , Stroke(..)
  , zostroke
  , zcstroke

  , Fill(..)
  , zfill
  
  , clip

  , TextLabel(..)
  , ztextlabel
  , multilabel

  , Ellipse(..)
  , zellipse




  -- * Operations
  , extendBoundary

  ) where

import Wumpus.Core.BoundingBox
import Wumpus.Core.Colour
import Wumpus.Core.Geometry
import Wumpus.Core.GraphicsState
import Wumpus.Core.PictureInternal
import Wumpus.Core.Utils

import Data.Semigroup




--------------------------------------------------------------------------------

-- Default attributes

psBlack :: PSColour
psBlack = PSRgb 0 0 0
 
-- aka the standard frame
stdFrame :: Num u => Frame2 u 
stdFrame = ortho zeroPt

--------------------------------------------------------------------------------
-- OneList helper


-- | This module (Wumpus.Core.Picture) should be the only 
-- interface to the /outside world/ for creating 

--------------------------------------------------------------------------------
-- Construction



-- | Create a blank picture sized to the supplied bounding box.
-- This is useful for spacing rows or columns of pictures.
blankPicture :: Num u => BoundingBox u -> Picture u
blankPicture bb = PicBlank (stdFrame, bb)

-- | Lift a Primitive to a Picture, located in the standard frame.
frame :: (Num u, Ord u) => Primitive u -> Picture u
frame p = Single (stdFrame, boundary p) p 

-- | Frame a picture within the supplied bounding box
-- 
-- A text label uses the supplied bounding box as is - no 
-- clipping is performed if the bounding box is 
-- smaller than the boundary size of the text. This may 
-- cause strange overlap for subsequent composite pictures, and
-- incorrect bounding box annotations in the prologue of the 
-- generated EPS file. 
-- 
-- Paths and ellipses are bound within the union of the supplied 
-- bounding box and the inherent bounding box or the path or 
-- ellipse. Thus the bounding box will never reframed to a 
-- smaller size than the /natural/ bounding box.
--
frameWithin :: (Num u, Ord u) => Primitive u -> BoundingBox u -> Picture u
frameWithin p@(PLabel _ _) bb = Single (stdFrame,bb) p
frameWithin p              bb = Single (stdFrame,bb `append` boundary p) p




-- | Lift a list of Primitives to a composite Picture, all 
-- Primitives will be located within the standard frame.
-- The list of Primitives must be non-empty.
--
frameMulti :: (Num u, Ord u) => [Primitive u] -> Picture u
frameMulti [] = error "Wumpus.Core.Picture.frameMulti - empty list"
frameMulti xs = multi $ map frame xs


-- | Place multiple pictures within the same affine frame
-- This function throws an error when supplied the empty list.
multi :: (Num u, Ord u) => [Picture u] -> Picture u
multi ps = Picture (stdFrame, sconcat $ map boundary ps) ones
  where 
    sconcat []      = error err_msg
    sconcat (x:xs)  = foldr append x xs

    ones            = fromListErr err_msg ps

    err_msg         = "Wumpus.Core.Picture.multi - empty list"



-- | Convert the list of vertices to a path of straight line 
-- segments.
vertexPath :: [Point2 u] -> Path u
vertexPath []     = error "Picture.vertexPath - empty point list"
vertexPath (x:xs) = Path x (map PLine xs)


-- Not a paramorphism as you want to consume3 rather than 
-- look-ahead3...

-- | Convert a list of vertices to a path of curve segments.
-- The first point in the list makes the start point, each curve 
-- segment thereafter takes 3 points. /Spare/ points at the end 
-- are discarded. 
curvedPath :: [Point2 u] -> Path u
curvedPath []     = error "Picture.curvedPath - empty point list"
curvedPath (x:xs) = Path x (fn xs) where
  fn (a:b:c:ys) = PCurve a b c : fn ys 
  fn _          = []


  



--------------------------------------------------------------------------------
-- Take Paths to Primitives


ostrokePath :: (Num u, Ord u) 
            => PSColour -> [StrokeAttr] -> Path u -> Primitive u
ostrokePath c attrs p = PPath (c, OStroke attrs) p

cstrokePath :: (Num u, Ord u) 
            => PSColour -> [StrokeAttr] -> Path u -> Primitive u
cstrokePath c attrs p = PPath (c, CStroke attrs) p

class Stroke t where
  ostroke :: (Num u, Ord u) => t -> Path u -> Primitive u
  cstroke :: (Num u, Ord u) => t -> Path u -> Primitive u

instance Stroke () where
  ostroke () = ostrokePath psBlack []
  cstroke () = cstrokePath psBlack []

instance Stroke PSColour where
  ostroke c = ostrokePath c []
  cstroke c = cstrokePath c []

instance Stroke (RGB3 Double) where
  ostroke c = ostrokePath (toPSColour c) []
  cstroke c = cstrokePath (toPSColour c) []

instance Stroke (HSB3 Double) where
  ostroke c = ostrokePath (toPSColour c) []
  cstroke c = cstrokePath (toPSColour c) []

instance Stroke (Gray Double) where
  ostroke c = ostrokePath (toPSColour c) []
  cstroke c = cstrokePath (toPSColour c) []


instance Stroke StrokeAttr where
  ostroke x = ostrokePath psBlack [x]
  cstroke x = cstrokePath psBlack [x]

instance Stroke [StrokeAttr] where
  ostroke xs = ostrokePath psBlack xs
  cstroke xs = cstrokePath psBlack xs


instance Stroke (PSColour,StrokeAttr) where
  ostroke (c,x) = ostrokePath c [x]
  cstroke (c,x) = cstrokePath c [x]

instance Stroke (RGB3 Double,StrokeAttr) where
  ostroke (c,x) = ostrokePath (toPSColour c) [x]
  cstroke (c,x) = cstrokePath (toPSColour c) [x]

instance Stroke (HSB3 Double,StrokeAttr) where
  ostroke (c,x) = ostrokePath (toPSColour c) [x]
  cstroke (c,x) = cstrokePath (toPSColour c) [x]

instance Stroke (Gray Double,StrokeAttr) where
  ostroke (c,x) = ostrokePath (toPSColour c) [x]
  cstroke (c,x) = cstrokePath (toPSColour c) [x]

instance Stroke (PSColour,[StrokeAttr]) where
  ostroke (c,xs) = ostrokePath c xs
  cstroke (c,xs) = cstrokePath c xs

instance Stroke (RGB3 Double,[StrokeAttr]) where
  ostroke (c,xs) = ostrokePath (toPSColour c) xs
  cstroke (c,xs) = cstrokePath (toPSColour c) xs

instance Stroke (HSB3 Double,[StrokeAttr]) where
  ostroke (c,xs) = ostrokePath (toPSColour c) xs
  cstroke (c,xs) = cstrokePath (toPSColour c) xs

instance Stroke (Gray Double,[StrokeAttr]) where
  ostroke (c,xs) = ostrokePath (toPSColour c) xs
  cstroke (c,xs) = cstrokePath (toPSColour c) xs


-- | Create an open stoke coloured black.
zostroke :: (Num u, Ord u) => Path u -> Primitive u
zostroke = ostrokePath psBlack []
 
-- | Create a closed stroke coloured black.
zcstroke :: (Num u, Ord u) => Path u -> Primitive u
zcstroke = cstrokePath psBlack []




-- fills only have one property - colour
-- Having a fill class seems uniform as we have a stroke class 



fillPath :: (Num u, Ord u) => PSColour -> Path u -> Primitive u
fillPath c p = PPath (c,CFill) p

class Fill t where
  fill :: (Num u, Ord u) => t -> Path u -> Primitive u
 

instance Fill ()                where fill () = fillPath psBlack 
instance Fill PSColour          where fill = fillPath
instance Fill (RGB3 Double)     where fill = fillPath . toPSColour
instance Fill (HSB3 Double)     where fill = fillPath . toPSColour
instance Fill (Gray Double)     where fill = fillPath . toPSColour

-- | Create a filled path coloured black. 
zfill :: (Num u, Ord u) => Path u -> Primitive u
zfill = fillPath psBlack

--------------------------------------------------------------------------------
-- Clipping 

clip :: (Num u, Ord u) => Path u -> Picture u -> Picture u
clip cp p = Clip (ortho zeroPt, boundary cp) cp p


--------------------------------------------------------------------------------
-- Labels to primitive

mkTextLabel :: PSColour -> FontAttr -> Point2 u -> String -> Primitive u
mkTextLabel c attr pt txt = PLabel (c,attr) (Label pt txt)

-- SVG seems to have an issue with /Courier/ and needs /Courier New/.

default_font :: FontAttr
default_font = FontAttr "Courier" "Courier New" SVG_REGULAR 12

class TextLabel t where 
  textlabel :: t -> Point2 u -> String -> Primitive u


instance TextLabel () where textlabel () = mkTextLabel psBlack default_font

instance TextLabel PSColour where
  textlabel c = mkTextLabel c default_font

instance TextLabel (RGB3 Double) where
  textlabel c = mkTextLabel (toPSColour c) default_font

instance TextLabel (HSB3 Double) where
  textlabel c = mkTextLabel (toPSColour c) default_font

instance TextLabel (Gray Double) where
  textlabel c = mkTextLabel (toPSColour c) default_font

instance TextLabel FontAttr where
  textlabel a = mkTextLabel psBlack a

instance TextLabel (PSColour,FontAttr) where
  textlabel (c,a) = mkTextLabel c a

instance TextLabel (RGB3 Double,FontAttr) where
  textlabel (c,a) = mkTextLabel (toPSColour c) a

instance TextLabel (HSB3 Double,FontAttr) where
  textlabel (c,a) = mkTextLabel (toPSColour c) a

instance TextLabel (Gray Double,FontAttr) where
  textlabel (c,a) = mkTextLabel (toPSColour c) a

-- | Create a label where the font is @Courier@, text size is 10 
-- and colour is black.
ztextlabel :: Point2 u -> String -> Primitive u
ztextlabel = mkTextLabel psBlack default_font



-- (The implementation of this function needs attention).
--
multilabel :: (Num u, Ord u) 
           => [Label u] -> LabelProps -> BoundingBox u -> Picture u
multilabel ps props bb = 
    Picture (stdFrame, bb) $ fromListErr err_msg 
                           $ map frame
                           $ zipWith PLabel (repeat props) ps 
  where 
    err_msg = "Wumpus.Core.Picture.multilabel - empty list."

--------------------------------------------------------------------------------

mkEllipse :: Num u => PSColour -> DrawEllipse -> Point2 u -> u -> u -> Primitive u
mkEllipse c dp pt hw hh = PEllipse (c,dp) pt hw hh


ellipseDefault :: EllipseProps
ellipseDefault = (psBlack, EFill)


-- | Instances will create a filled ellipse unless the supplied 
-- element /implies/ a stoked ellipse, e.g.:
--
-- > ellipse (LineWidth 4) zeroPt 40 40 
-- > ellipse EFill zeroPt 40 40  
--
class Ellipse t where
  ellipse :: Fractional u => t -> Point2 u -> u -> u -> Primitive u

instance Ellipse ()             where ellipse () = zellipse
instance Ellipse PSColour       where ellipse c  = mkEllipse c EFill
instance Ellipse DrawEllipse    where ellipse dp = mkEllipse psBlack dp

instance Ellipse StrokeAttr     where 
    ellipse = mkEllipse psBlack . EStroke . return

instance Ellipse [StrokeAttr]   where 
    ellipse = mkEllipse psBlack . EStroke

instance Ellipse (RGB3 Double) where 
    ellipse c = mkEllipse (toPSColour c) EFill

instance Ellipse (HSB3 Double) where 
    ellipse c = mkEllipse (toPSColour c) EFill

instance Ellipse (Gray Double) where 
    ellipse c = mkEllipse (toPSColour c) EFill


instance Ellipse (PSColour,DrawEllipse) where 
    ellipse (c,dp) = mkEllipse c dp

instance Ellipse (RGB3 Double,DrawEllipse) where 
    ellipse (c,dp) = mkEllipse (toPSColour c) dp

instance Ellipse (HSB3 Double,DrawEllipse) where 
    ellipse (c,dp) = mkEllipse (toPSColour c) dp

instance Ellipse (Gray Double,DrawEllipse) where 
    ellipse (c,dp) = mkEllipse (toPSColour c) dp


instance Ellipse (PSColour,StrokeAttr) where 
    ellipse (c,x) = mkEllipse c (EStroke [x])

instance Ellipse (RGB3 Double,StrokeAttr) where 
    ellipse (c,x) = mkEllipse (toPSColour c) (EStroke [x])

instance Ellipse (HSB3 Double,StrokeAttr) where 
    ellipse (c,x) = mkEllipse (toPSColour c) (EStroke [x])

instance Ellipse (Gray Double,StrokeAttr) where 
    ellipse (c,x) = mkEllipse (toPSColour c) (EStroke [x])

instance Ellipse (PSColour,[StrokeAttr]) where 
    ellipse (c,xs) = mkEllipse c (EStroke xs)

instance Ellipse (RGB3 Double,[StrokeAttr]) where 
    ellipse (c,xs) = mkEllipse (toPSColour c) (EStroke xs)

instance Ellipse (HSB3 Double,[StrokeAttr]) where 
    ellipse (c,xs) = mkEllipse (toPSColour c) (EStroke xs)

instance Ellipse (Gray Double,[StrokeAttr]) where 
    ellipse (c,xs) = mkEllipse (toPSColour c) (EStroke xs)


-- | Create a black, filled ellipse. 
zellipse :: Num u => Point2 u -> u -> u -> Primitive u
zellipse = uncurry mkEllipse ellipseDefault


--------------------------------------------------------------------------------

-- Operations on pictures and paths




-- | Extend the bounding box of a picture. 
extendBoundary :: Num u => u -> u -> Picture u -> Picture u
extendBoundary x y = mapLocale (\(fr,bb) -> (fr, extBB bb)) 
  where
    extBB (BBox (P2 x0 y0) (P2 x1 y1)) = BBox pt1 pt2 where 
        pt1 = P2 (x0-x) (y0-y)
        pt2 = P2 (x1+x) (y1+y)

