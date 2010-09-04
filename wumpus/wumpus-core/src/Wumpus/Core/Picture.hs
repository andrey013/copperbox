{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.Picture
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Construction of pictures, paths and text labels.
--
--------------------------------------------------------------------------------


module Wumpus.Core.Picture
  ( 

  -- * Construction
    frame
  , multi
  , renderContext
  , path
  , lineTo
  , curveTo
  , vertexPath
  , curvedPath
  , xlinkhref

  -- * Constructing primitives
  , ostroke
  , cstroke
  , xostroke
  , xcstroke
  , zostroke
  , zcstroke

  , fill
  , xfill
  , zfill

  , bordered
  , xbordered
  , clip

  , textlabel
  , xtextlabel
  , ztextlabel
  , wumpus_default_font

  , strokeEllipse
  , fillEllipse
  , xstrokeEllipse
  , xfillEllipse
  , zellipse
  , borderedEllipse
  
  -- * SVG \'delta' optimizations
  , composeDelta
  , deltaFontSize 
  , deltaFontFace

  -- * Operations
  , extendBoundary  

  -- * Picture composition
  , picOver
  , picMoveBy
  , picBeside

  -- * Illustrating pictures and primitives
  , printPicture
  , illustrateBounds
  , illustrateBoundsPrim
  , illustrateControlPoints

  ) where

import Wumpus.Core.AffineTrans
import Wumpus.Core.BoundingBox
import Wumpus.Core.Colour
import Wumpus.Core.FormatCombinators hiding ( fill )
import Wumpus.Core.Geometry
import Wumpus.Core.GraphicsState
import Wumpus.Core.OneList
import Wumpus.Core.PictureInternal
import Wumpus.Core.PtSize
import Wumpus.Core.TextInternal
import Wumpus.Core.Utils

import Data.AffineSpace                         -- package: vector-space
import Data.Semigroup                           -- package: algebra




--------------------------------------------------------------------------------
-- Construction


-- | Lift a list of primitives to a composite picture.
--
-- The order of the list maps to the zorder - the front of the
-- list is drawn at the top.
--
-- This function throws an error when supplied the empty list.
--
frame :: (Real u, Floating u, FromPtSize u) => [Primitive u] -> Picture u
frame []     = error "Wumpus.Core.Picture.frame - empty list"
frame (p:ps) = let (bb,ones) = step p ps in Leaf (bb,[]) ones 
  where
    step a []     = (boundary a, one a)
    step a (x:xs) = let (bb', rest) = step x xs
                    in ( boundary a `append` bb', cons a rest )



-- | Place multiple pictures within the standard affine frame.
--
-- This function throws an error when supplied the empty list.
--
multi :: (Fractional u, Ord u) => [Picture u] -> Picture u
multi []      = error "Wumpus.Core.Picture.multi - empty list"
multi (p:ps)  = let (bb,ones) = step p ps in Picture (bb,[]) ones 
  where
    step a []     = (boundary a, one a)
    step a (x:xs) = let (bb', rest) = step x xs
                    in ( boundary a `append` bb', cons a rest )


-- | Update the rendering graphics state for SVG output.
--
-- Note - 'renderWith' does not set the graphics properties of
-- elements in the supplied Picture, it is solely a mechanism to
-- help reduce the code size of the generated SVG by factoring 
-- common attributes into a group (g) element. For instance, 
-- settting the font properties with renderWidth can eliminate 
-- the repeated use of font-family and font-size in this code:
--
-- > <text font-family="Helvetica" font-size="12"> ... </text>
-- > <text font-family="Helvetica" font-size="12"> ... </text>
-- > <text font-family="Helvetica" font-size="12"> ... </text>
--
-- With the appropriate GSUpdate function, this code will be
-- generated:
-- 
-- > <g font-family="Helvetica" font-size="12">
-- >   <text > ... </text>
-- >   <text > ... </text>
-- >   <text > ... </text>
-- > </g>
--
-- Wumpus ignores 'renderWith' directives when generating 
-- PostScript. Unlike SVG, PostScript is not naturally nested, so 
-- introducing nesting with @gsave@ and @grestore@ is not likely
-- to improve the code Wumpus generates.
--
renderContext :: GSUpdate -> Picture u -> Picture u
renderContext upd p = Group (boundary p, []) upd p


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


-- | Constructor for SVG hyperlinks.
--
xlinkhref :: String -> XLink
xlinkhref = XLinkHRef


--------------------------------------------------------------------------------
-- 

-- Design issue.
--
-- The overloading style below - patterned after Iavor S. 
-- Diatchki\'s XML-Light - is (probably) not as valuable as first 
-- anticipated. 
--
-- Wumpus-Basic takes graphic styles from a reader monad, so it 
-- \'always\' calls the overloaded operations with the same 
-- argument types and therefore doesn\'t need overloading.
--
-- It\'s perhaps unlikely that any other software would want to 
-- use Wumpus-Core directly, if it did then it is quite possible 
-- the exact argument types would again be uniform and no
-- shorthand via overloading would be necessary.
--
--


--------------------------------------------------------------------------------
-- Take Paths to Primitives

-- *** Stroke

-- | Create a open, stroked path.
--
ostroke :: Num u 
        => RGBi -> StrokeAttr -> PrimPath u -> Primitive u
ostroke rgb sa p = PPath (OStroke sa rgb) NoLink p


-- | Create a closed, stroked path.
--
cstroke :: Num u 
        => RGBi -> StrokeAttr -> PrimPath u -> Primitive u
cstroke rgb sa p = PPath (CStroke sa rgb) NoLink p

-- | Create a open, stroked path with a hyperlink.
--
-- Note - hyperlinks are only rendered in SVG output.
--
xostroke :: Num u 
         => RGBi -> StrokeAttr -> XLink -> PrimPath u -> Primitive u
xostroke rgb sa xlink p = PPath (OStroke sa rgb) xlink p


-- | Create a closed, stroked path with a hyperlink.
--
--  Note - hyperlinks are only rendered in SVG output.
--
xcstroke :: Num u 
         => RGBi -> StrokeAttr -> XLink -> PrimPath u -> Primitive u
xcstroke rgb sa xlink p = PPath (CStroke sa rgb) xlink p




-- | Create an open, stroked path using the default stroke 
-- attributes and coloured black.
--
zostroke :: Num u => PrimPath u -> Primitive u
zostroke = ostroke black default_stroke_attr
 
-- | Create a closed stroked path using the default stroke 
-- attributes and coloured black.
--
zcstroke :: Num u => PrimPath u -> Primitive u
zcstroke = cstroke black default_stroke_attr

--------------------------------------------------------------------------------
-- *** Fill


-- | Create a filled path.
--
fill :: Num u => RGBi -> PrimPath u -> Primitive u
fill rgb p = PPath (CFill rgb) NoLink p


-- | Create a filled path with a hyperlink.
--
--  Note - hyperlinks are only rendered in SVG output.
--
xfill :: Num u => RGBi -> XLink -> PrimPath u -> Primitive u
xfill rgb xlink p = PPath (CFill rgb) xlink p

-- | Create a filled path coloured black. 
zfill :: Num u => PrimPath u -> Primitive u
zfill = fill black


--------------------------------------------------------------------------------
-- Bordered (closed) paths


-- | Create a closed path that is both filled and stroked (the fill
-- is below in the zorder).
--
-- > fill colour * stroke attrs * stroke_colour * ...
--
bordered :: Num u 
        => RGBi -> StrokeAttr -> RGBi -> PrimPath u -> Primitive u
bordered frgb sa srgb p = PPath (CFillStroke frgb sa srgb) NoLink p


-- | Create a bordered, closed path with a hyperlink.
--
--  Note - hyperlinks are only rendered in SVG output.
--
xbordered :: Num u 
        => RGBi -> StrokeAttr -> RGBi -> XLink -> PrimPath u -> Primitive u
xbordered frgb sa srgb xlink p = PPath (CFillStroke frgb sa srgb) xlink p



--------------------------------------------------------------------------------
-- Clipping 

-- | Clip a picture with respect to the supplied path.
--
clip :: (Num u, Ord u) => PrimPath u -> Picture u -> Picture u
clip cp p = Clip (pathBoundary cp, []) cp p

--------------------------------------------------------------------------------
-- Labels to primitive

-- | Create a text label. The string should not contain newline
-- or tab characters.
--
-- The supplied point is the left baseline.
--
textlabel :: Num u 
          => RGBi -> FontAttr -> String -> Point2 u -> Primitive u
textlabel rgb attr txt = xtextlabel rgb attr NoLink txt 


-- | Create a text label with a hyperlink. The string should not 
-- contain newline or tab characters.
--
-- The supplied point is the left baseline.
--
-- Note - hyperlinks are only rendered in SVG output.
--
xtextlabel :: Num u 
           => RGBi -> FontAttr -> XLink -> String -> Point2 u -> Primitive u
xtextlabel rgb attr xlink txt pt = PLabel (LabelProps rgb attr) xlink lbl 
  where
    lbl = PrimLabel pt (lexLabel txt) identityCTM


-- | Create a label where the font is @Courier@, text size is 14pt
-- and colour is black.
--
ztextlabel :: Num u => String -> Point2 u -> Primitive u
ztextlabel = xtextlabel black wumpus_default_font NoLink


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



--------------------------------------------------------------------------------


-- | Create a stroked ellipse.
--
-- Note - within Wumpus, ellipses are considered an unfortunate
-- but useful /optimization/. Drawing good cicles with Beziers 
-- needs at least eight curves, but drawing them with 
-- PostScript\'s @arc@ command needs a single operation. For 
-- drawings with many dots (e.g. scatter plots) it seems sensible
-- to employ this optimization.
--
-- A deficiency of Wumpus\'s ellipse is that (non-uniformly)
-- scaling a stroked ellipse also (non-uniformly) scales the pen 
-- it is drawn with. Where the ellipse is wider, the pen stroke 
-- will be wider too. 
--
-- Avoid non-uniform scaling stroked ellipses!
--
strokeEllipse :: Num u 
             => RGBi -> StrokeAttr -> u -> u -> Point2 u -> Primitive u
strokeEllipse rgb sa = xstrokeEllipse rgb sa NoLink


-- | Create a filled ellipse.
--
fillEllipse :: Num u 
             => RGBi -> u -> u -> Point2 u -> Primitive u
fillEllipse rgb = xfillEllipse rgb NoLink

-- | Create a stroked ellipse with a hyperlink.
--
-- Note - hyperlinks are only rendered in SVG output.
--
xstrokeEllipse :: Num u 
             => RGBi -> StrokeAttr -> XLink -> u -> u -> Point2 u -> Primitive u
xstrokeEllipse rgb sa xlink hw hh pt = 
    PEllipse (EStroke sa rgb) xlink (PrimEllipse pt hw hh identityCTM)

-- | Create a filled ellipse.
--
-- Note - hyperlinks are only rendered in SVG output.
-- 
xfillEllipse :: Num u 
             => RGBi -> XLink -> u -> u -> Point2 u -> Primitive u
xfillEllipse rgb xlink hw hh pt = 
    PEllipse (EFill rgb) xlink (PrimEllipse pt hw hh identityCTM)


-- | Create a black, filled ellipse. 
zellipse :: Num u => u -> u -> Point2 u -> Primitive u
zellipse hw hh pt = xfillEllipse black NoLink hw hh pt


-- | Create a bordered (i.e. filled and stroked) ellipse.
--
borderedEllipse :: Num u 
                => RGBi -> StrokeAttr -> RGBi -> u -> u -> Point2 u 
                -> Primitive u
borderedEllipse frgb sa srgb = xborderedEllipse frgb sa srgb NoLink

-- | Create a bordered (i.e. filled and stroked) ellipse with a 
-- hyperlink.
--
-- Note - hyperlinks are only rendered in SVG output.
--
xborderedEllipse :: Num u 
                 => RGBi -> StrokeAttr -> RGBi -> XLink -> u -> u -> Point2 u 
                 -> Primitive u
xborderedEllipse frgb sa srgb xlink hw hh pt = 
    PEllipse (EFillStroke frgb sa srgb) xlink (PrimEllipse pt hw hh identityCTM)


--------------------------------------------------------------------------------
-- SVG delta operations

-- | Compose SVG /delta/ updates to the rendering state.
--
composeDelta :: GSUpdate -> GSUpdate -> GSUpdate
composeDelta a b = GSUpdate $ getGSU a . getGSU b

-- | Set the /delta/ font size attribute. 
--
-- For SVG Ouput, text elements within the modified rendering 
-- context will only be annotated with a @font-size@ attribute 
-- if the size differs from the /delta/ size in the enclosing
-- group (g) element.
--
-- Note - there is no /inheritance/ of drawing styles in
-- Wumpus-Core. Setting the delta font size only changes 
-- (optimizes) how the SVG is generated; it does not change the 
-- size of any text elements.
--
deltaFontSize :: Int -> GSUpdate
deltaFontSize i = GSUpdate $ \s -> s { gs_font_size = i }


-- | Set the /delta/ font face attribute. 
--
-- For SVG Ouput, text elements within the modified rendering 
-- context will only be annotated with a @font-family@, 
-- @font-weight@ and @font-style@ attributes if the differ from 
-- the /deltas/ in the enclosing group (g) element.
--
-- Note - there is no /inheritance/ of drawing styles in
-- Wumpus-Core. Setting the delta font face only changes 
-- (optimizes) how the SVG is generated; it does not change the 
-- font properties of any text elements.
--
deltaFontFace :: FontFace -> GSUpdate
deltaFontFace i = GSUpdate $ \s -> s { gs_font_face = i }


-- Note - stroke and colour attributes - cannot be set. 
--
-- Doing so would be likely to produce \*worse\* SVG, i.e. 
-- Wumpus will find more differences with the enclosing group 
-- (g) element and have to add more attributes to child nodes.
--  

--------------------------------------------------------------------------------
-- Operations

-- | Extend the bounding box of a picture. 
--
-- The bounding box is both horizontal directions by @x@ and 
-- both vertical directions by @y@. @x@ and @y@ must be positive
-- This function cannot be used to shrink a boundary.
--
extendBoundary :: (Num u, Ord u) => u -> u -> Picture u -> Picture u
extendBoundary x y = mapLocale (\(bb,xs) -> (extBB (posve x) (posve y) bb, xs)) 
  where
    extBB x' y' (BBox (P2 x0 y0) (P2 x1 y1)) = BBox pt1 pt2 where 
        pt1 = P2 (x0-x') (y0-y')
        pt2 = P2 (x1+x') (y1+y')
    
    posve n | n < 0     = 0
            | otherwise = n 

--------------------------------------------------------------------------------
-- Minimal support for Picture composition

infixr 6 `picBeside`, `picOver`

-- | 'picOver' : @ picture -> picture -> picture @
--
-- Draw the first picture on top of the second picture - 
-- neither picture will be moved.
--
picOver :: (Num u, Ord u) => Picture u -> Picture u -> Picture u
a `picOver` b = Picture (bb,[]) (cons a $ one b)
  where
    bb = boundary a `append` boundary b

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
    (P2 x1 _) = ur_corner $ boundary a
    (P2 x2 _) = ll_corner $ boundary b 
    v         = hvec $ x1 - x2 

--------------------------------------------------------------------------------
-- Illustrating pictures and primitives

-- | Print the syntax tree of a Picture to the console.
--
printPicture :: (Num u, PSUnit u) => Picture u -> IO ()
printPicture pic = putStrLn (show $ format pic) >> putStrLn []


-- | 'illustrateBounds' : @ colour -> picture -> picture @
-- 
-- Draw the picture on top of an image of its bounding box.
-- The bounding box image will be drawn in the supplied colour.
--
illustrateBounds :: (Real u, Floating u, FromPtSize u) 
                 => RGBi -> Picture u -> Picture u
illustrateBounds rgb p = p `picOver` (frame $ boundsPrims rgb p) 


-- | 'illustrateBoundsPrim' : @ colour -> primitive -> picture @
-- 
-- Draw the primitive on top of an image of its bounding box.
-- The bounding box image will be drawn in the supplied colour.
--
-- The result will be lifted from Primitive to Picture.
-- 
illustrateBoundsPrim :: (Real u, Floating u, FromPtSize u) 
                     => RGBi -> Primitive u -> Picture u
illustrateBoundsPrim rgb p = frame (p : boundsPrims rgb p)



-- | Draw a the rectangle of a bounding box, plus cross lines
-- joining the corners.
--
boundsPrims :: (Num u, Ord u, Boundary t, u ~ DUnit t) 
            => RGBi -> t -> [Primitive u]
boundsPrims rgb a = [ bbox_rect, bl_to_tr, br_to_tl ]
  where
    (bl,br,tr,tl) = boundaryCorners $ boundary a
    bbox_rect     = cstroke rgb line_attr $ vertexPath [bl,br,tr,tl]
    bl_to_tr      = ostroke rgb line_attr $ vertexPath [bl,tr]
    br_to_tl      = ostroke rgb line_attr $ vertexPath [br,tl]

    line_attr     = default_stroke_attr { line_cap     = CapRound
                                        , dash_pattern = Dash 0 [(1,2)] }


-- | Generate the control points illustrating the Bezier 
-- curves within a picture.
-- 
-- This has no effect on TextLabels.
-- 
-- Pseudo control points are generated for ellipses, 
-- although strictly speaking ellipses do not use Bezier
-- curves - they are implemented with PostScript\'s 
-- @arc@ command.  
--
illustrateControlPoints :: (Real u, Floating u, FromPtSize u)
                        => RGBi -> Primitive u -> Picture u
illustrateControlPoints rgb prim = step prim
  where
    step (PEllipse _ _ e) = frame (prim : ellipseCtrlLines rgb e)
    step (PPath    _ _ p) = frame (prim : pathCtrlLines rgb p)
    step _                = frame [prim]

-- Genrate lines illustrating the control points of curves on 
-- a Path.
--
-- Two lines are generated for a Bezier curve:
-- start-point to control-point1; control-point2 to end-point
--
-- Nothing is generated for a straight line.
--
pathCtrlLines :: (Num u, Ord u) => RGBi -> PrimPath u -> [Primitive u]
pathCtrlLines rgb (PrimPath start ss) = step start ss
  where 
    -- trail the current end point through the recursion...
    step _ []                    = []
    step _ (PLineTo e:xs)        = step e xs
    step s (PCurveTo c1 c2 e:xs) = mkLine s c1 : mkLine c2 e : step e xs 

    mkLine s e                   = let pp = (PrimPath s [lineTo e]) in 
                                   ostroke rgb default_stroke_attr pp 


-- Generate lines illustrating the control points of an 
-- ellipse:
-- 
-- Two lines for each quadrant: 
-- start-point to control-point1; control-point2 to end-point
--
ellipseCtrlLines :: (Real u, Floating u) 
                 => RGBi -> PrimEllipse u -> [Primitive u]
ellipseCtrlLines rgb pe = start all_points
  where 
    -- list in order: 
    -- [s,cp1,cp2,e, cp1,cp2,e, cp1,cp2,e, cp1,cp2,e]

    all_points           = ellipseControlPoints pe

    start (s:c1:c2:e:xs) = mkLine s c1 : mkLine c2 e : rest e xs
    start _              = []

    rest s (c1:c2:e:xs)  = mkLine s c1 : mkLine c2 e : rest e xs
    rest _ _             = []

    mkLine s e  = ostroke rgb default_stroke_attr (PrimPath s [lineTo e]) 



-- | Get the control points as a list
-- 
-- There are no duplicates in the list except for the final 
-- /wrap-around/. We take 4 points initially (start,cp1,cp2,end)
-- then (cp1,cp2,end) for the other three quadrants.
--
ellipseControlPoints :: (Floating u, Real u)
                     => PrimEllipse u -> [Point2 u]
ellipseControlPoints (PrimEllipse (P2 x y) hw hh ctm) = 
    map (disp . (new_mtrx *#)) circ
  where
    disp             = (.+^ V2 x y)
    (radius,(dx,dy)) = circleScalingProps hw hh
    new_mtrx         = matrixRepCTM $ scaleCTM dx dy ctm
    circ             = bezierCircle 1 radius (P2 0 0)

    -- subdivide the bezierCircle with 1 to get two
    -- control points per quadrant.    


--
-- I don't know how to calculate bezier arcs (and thus control
-- points) for an ellipse but I know how to do it for a circle...
--
-- So a make a circle with the largest of half-width and 
-- half-height then apply a scale to the points
-- 
circleScalingProps  :: (Fractional u, Ord u) => u -> u -> (u,(u,u))
circleScalingProps hw hh  = (radius, (dx,dy))
  where
    radius     = max hw hh
    (dx,dy)    = if radius == hw then (1, rescale (0,hw) (0,1) hh)
                                 else (rescale (0,hh) (0,1) hw, 1)

