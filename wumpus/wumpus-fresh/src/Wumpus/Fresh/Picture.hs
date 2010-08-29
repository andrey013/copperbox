{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Fresh.Picture
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


module Wumpus.Fresh.Picture
  ( 

    path
  , lineTo
  , curveTo
  , vertexPath
  , curvedPath

  , Stroke(..)
  , zostroke
  , zcstroke
  , Fill(..)
  , zfill

  , clip

  , TextLabel(..)

  , frameMulti
  , ellipse_
  , printPicture

  , picOver
  , picMoveBy
  , picBeside

  ) where

import Wumpus.Fresh.AffineTrans
import Wumpus.Fresh.BoundingBox
import Wumpus.Fresh.Colour
import Wumpus.Fresh.FormatCombinators
import Wumpus.Fresh.Geometry
import Wumpus.Fresh.GraphicsState
import Wumpus.Fresh.OneList
import Wumpus.Fresh.PictureInternal
import Wumpus.Fresh.PtSize
import Wumpus.Fresh.TextInternal
import Wumpus.Fresh.Utils

import Data.Semigroup                           -- package: algebra




--------------------------------------------------------------------------------
-- Constructors 

-- | Create a Path from a start point and a list of PathSegments.
--
path :: Point2 u -> [PrimPathSegment u] -> PrimPath u
path = PrimPath 

-- | Create a straight-line PathSegment.
--
lineTo :: Point2 u -> PrimPathSegment u
lineTo = PLineTo

-- | Create a curved PathSegment.
--
curveTo :: Point2 u -> Point2 u -> Point2 u -> PrimPathSegment u
curveTo = PCurveTo


-- | Convert the list of vertices to a path of straight line 
-- segments.
--
vertexPath :: [Point2 u] -> PrimPath u
vertexPath []     = error "Picture.vertexPath - empty point list"
vertexPath (x:xs) = PrimPath x (map PLineTo xs)



-- | Convert a list of vertices to a path of curve segments.
-- The first point in the list makes the start point, each curve 
-- segment thereafter takes 3 points. /Spare/ points at the end 
-- are discarded. 
--
curvedPath :: [Point2 u] -> PrimPath u
curvedPath []     = error "Picture.curvedPath - empty point list"
curvedPath (x:xs) = PrimPath x (step xs) 
  where
    step (a:b:c:ys) = PCurveTo a b c : step ys 
    step _          = []


--------------------------------------------------------------------------------
-- Take Paths to Primitives

-- *** Stroke

ostrokePath :: Num u => RGB255 -> [StrokeAttr] -> PrimPath u -> Primitive u
ostrokePath rgb attrs p = PPath (OStroke attrs rgb) NoLink p

cstrokePath :: Num u => RGB255 -> [StrokeAttr] -> PrimPath u -> Primitive u
cstrokePath rgb attrs p = PPath (CStroke attrs rgb) NoLink p

-- | Create a open, stroked path (@ostroke@) or a closed, stroked
-- path (@cstroke@).
--
-- @ostroke@ and @cstroke@ are overloaded to make attributing 
-- the path more convenient.
-- 
class Stroke t where
  ostroke :: Num u => t -> PrimPath u -> Primitive u
  cstroke :: Num u => t -> PrimPath u -> Primitive u

instance Stroke () where
  ostroke () = ostrokePath black []
  cstroke () = cstrokePath black []

instance Stroke RGB255 where
  ostroke rgb = ostrokePath rgb []
  cstroke rgb = cstrokePath rgb []

instance Stroke StrokeAttr where
  ostroke x = ostrokePath black [x]
  cstroke x = cstrokePath black [x]

instance Stroke [StrokeAttr] where
  ostroke xs = ostrokePath black xs
  cstroke xs = cstrokePath black xs



instance Stroke (RGB255,StrokeAttr) where
  ostroke (rgb,x) = ostrokePath rgb [x]
  cstroke (rgb,x) = cstrokePath rgb [x]


instance Stroke (RGB255,[StrokeAttr]) where
  ostroke (rgb,xs) = ostrokePath rgb xs
  cstroke (rgb,xs) = cstrokePath rgb xs

-- | Create an open stoke coloured black.
--
zostroke :: Num u => PrimPath u -> Primitive u
zostroke = ostrokePath black []
 
-- | Create a closed stroke coloured black.
--
zcstroke :: Num u => PrimPath u -> Primitive u
zcstroke = cstrokePath black []


-- *** Fill

fillPath :: Num u => RGB255 -> PrimPath u -> Primitive u
fillPath rgb p = PPath (CFill rgb) NoLink p

-- | Create a filled path (@fill@). Fills only have one 
-- property - colour. But there are various representations of 
-- colour.
--
-- @ fill () @ will fill with the default colour - black.
-- 
class Fill t where
  fill :: Num u => t -> PrimPath u -> Primitive u
 

instance Fill ()                where fill () = fillPath black
instance Fill RGB255            where fill = fillPath

-- | Create a filled path coloured black. 
zfill :: Num u => PrimPath u -> Primitive u
zfill = fillPath black

--------------------------------------------------------------------------------
-- Clipping 

-- | Clip a picture with respect to the supplied path.
--
clip :: (Num u, Ord u) => PrimPath u -> Picture u -> Picture u
clip cp p = Clip (pathBoundary cp,[]) cp p

--------------------------------------------------------------------------------
-- Labels to primitive

mkTextLabel :: Num u 
            => RGB255 -> FontAttr -> String -> Point2 u -> Primitive u
mkTextLabel rgb attr txt pt = PLabel (LabelProps rgb attr) NoLink lbl 
  where
    lbl = PrimLabel pt (lexLabel txt) identityCTM

-- | Constant for the default font, which is @Courier@ (aliased 
-- to @Courier New@ for SVG) at 14 point.
--
--
wumpus_default_font :: FontAttr
wumpus_default_font = FontAttr 14 face 
  where
    face = FontFace { font_name         = "Courier"
                    , svg_font_family   = "Courier New"
                    , svg_font_style    = SVG_REGULAR
                    }


-- | Create a text label. The string should not contain newline
-- or tab characters. Use 'multilabel' to create text with 
-- multiple lines.
-- 
-- @textlabel@ is overloaded to make attributing the label more 
-- convenient.
--
-- Unless a 'FontAttr' is specified, the label will use 12pt 
-- Courier.
--
-- The supplied point is is the bottom left corner.
--
class TextLabel t where 
  textlabel :: Num u => t -> String -> Point2 u -> Primitive u


instance TextLabel () where 
    textlabel () = mkTextLabel black wumpus_default_font

instance TextLabel RGB255 where
  textlabel rgb = mkTextLabel rgb wumpus_default_font

instance TextLabel FontAttr where
  textlabel a = mkTextLabel black a

instance TextLabel (RGB255,FontAttr) where
  textlabel (rgb,a) = mkTextLabel rgb a



--------------------------------------------------------------------------------
-- This function throws an error when supplied the empty list.
--
frameMulti :: (Real u, Floating u, FromPtSize u) 
           => [Primitive u] -> Picture u
frameMulti []     = error "Wumpus.Core.Picture.frameMulti - empty list"
frameMulti (p:ps) = let (bb,ones) = step p ps 
                    in Leaf (bb,[]) ones 
  where
    step a []     = (boundary a, one a)
    step a (x:xs) = let (bb',rest) = step x xs
                    in (boundary a `append` bb', cons a rest)


ellipse_ :: Num u => u -> u -> Point2 u -> Primitive u
ellipse_ hw hh pt = PEllipse (EFill (RGB255 127 0 0)) NoLink body
  where
    body = PrimEllipse { ellipse_center        = pt
                       , ellipse_half_width    = hw
                       , ellipse_half_height   = hh
                       , ellipse_ctm           = identityCTM
                       } 


printPicture :: (Num u, PSUnit u) => Picture u -> IO ()
printPicture pic = putStrLn (show $ format pic) >> putStrLn []

--------------------------------------------------------------------------------
-- Minimal support for Picture composition

infixr 6 `picBeside`, `picOver`

-- | 'picOver' : @ picture -> picture -> picture @
--
-- Draw the first picture on top of the second picture - 
-- neither picture will be moved.
--
picOver :: (Num u, Ord u) => Picture u -> Picture u -> Picture u
a `picOver` b = Picture (bb, []) (cons a $ one b)
  where
    bb = (boundary a) `append` (boundary b)

-- | 'picMoveBy' : @ picture -> vector -> picture @
-- 
--  Move a picture by the supplied vector. 
--
picMoveBy :: (Num u, Ord u) => Picture u -> Vec2 u -> Picture u
p `picMoveBy` (V2 dx dy) = translate dx dy p 

-- | 'picBeside' : @ picture -> picture -> picture @
--
-- Move the second picture to sit at the right side of the
-- first picture
--
picBeside :: (Num u, Ord u) => Picture u -> Picture u -> Picture u
a `picBeside` b = a `picOver` (b `picMoveBy` v) 
  where 
    v = hvec $ boundaryWidth $ boundary a
