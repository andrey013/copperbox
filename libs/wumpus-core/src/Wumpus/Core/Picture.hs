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
    empty

  , frame
  , multi
  , reframe

  , Stroke(..)
  , zostroke
  , zcstroke

  , Fill(..)
  , zfill

  , TextLabel(..)
  , ztextlabel
  , multilabel

  , Ellipse(..)
  , zellipse


  , vertexPath  




  -- * Operations
  , nullPicture
  , extractFrame


  ) where

import Wumpus.Core.BoundingBox
import Wumpus.Core.Colour
import Wumpus.Core.Geometry
import Wumpus.Core.GraphicsState
import Wumpus.Core.PictureInternal

import Data.Monoid





--------------------------------------------------------------------------------

-- Default attributes

psBlack :: PSColour
psBlack = PSRgb 0 0 0
 
-- aka the standard frame
stdFrame :: Num u => Frame2 u 
stdFrame = ortho zeroPt




--------------------------------------------------------------------------------
-- Construction

empty :: Picture u
empty = PicEmpty

-- | Lifts primitives to Pictures...
frame :: (Num u, Ord u) => Primitive u -> Picture u
frame p = Single (stdFrame, boundary p) p 

multi :: (Num u, Ord u) => [Primitive u] -> Picture u
multi ps = Multi (stdFrame, mconcat $ map boundary ps) ps 


-- | Lift primitives to pictures modifying the bounding box.
reframe :: (Num u, Ord u) => Primitive u -> BoundingBox u -> Picture u
reframe p@(PLabel _ _) bb = Single (stdFrame,bb) p
reframe p              bb = Single (stdFrame,bb `mappend` boundary p) p



-- | Convert the list of vertices to a path of straight line 
-- segments.
vertexPath :: [Point2 u] -> Path u
vertexPath []     = error "straightLinePath - empty point list"
vertexPath (x:xs) = Path x (map PLine xs)




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
-- Labels to primitive

mkTextLabel :: PSColour -> FontAttr -> Point2 u -> String -> Primitive u
mkTextLabel c attr pt txt = PLabel (c,attr) (Label pt txt)

-- SVG seems to have an issue with /Courier/ and needs /Courier New/.

default_font :: FontAttr
default_font = FontAttr "Courier" "Courier New" 10

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



-- Note - this current definition never allows the bounding box
-- to be shrunk - a specific version for labels might be better
--
-- > multilabel :: (Num u, Ord u) => [Label u] -> BoundingBox u -> Picture u
-- 

multilabel :: (Num u, Ord u) 
           => [Label u] -> LabelProps -> BoundingBox u -> Picture u
multilabel ps props bb = Multi (stdFrame, bb) $ zipWith PLabel (repeat props) ps 


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


nullPicture :: Picture u -> Bool
nullPicture PicEmpty = True
nullPicture _        = False



extractFrame :: Num u => Picture u -> Frame2 u
extractFrame PicEmpty              = ortho zeroPt
extractFrame (PicBlank (fr,_))     = fr
extractFrame (Single   (fr,_) _)   = fr
extractFrame (Multi    (fr,_) _)   = fr
extractFrame (Picture  (fr,_) _ _) = fr
extractFrame (Clip     (fr,_) _ _) = fr




