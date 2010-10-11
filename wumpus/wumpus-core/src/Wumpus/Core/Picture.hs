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
  , fontDeltaContext
  , path
  , lineTo
  , curveTo
  , vertexPath
  , curvedPath
  , xlinkhref
  , xlinkGroup


  -- * Constructing primitives
  , ostroke
  , cstroke
  , zostroke
  , zcstroke

  , fill
  , zfill

  , fillStroke
  , clip

  , textlabel
  , ztextlabel
  , wumpus_default_font

  , hkernlabel
  , vkernlabel
  , kernchar
  , kernEscInt
  , kernEscName

  , strokeEllipse
  , fillEllipse
  , zellipse
  , fillStrokeEllipse
  

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
import Wumpus.Core.Geometry
import Wumpus.Core.GraphicsState
import Wumpus.Core.PictureInternal
import Wumpus.Core.PtSize
import Wumpus.Core.Text.TextInternal
import Wumpus.Core.Text.Latin1
import Wumpus.Core.TrafoInternal
import Wumpus.Core.Utils.Common
import Wumpus.Core.Utils.FormatCombinators hiding ( fill )
import Wumpus.Core.Utils.OneList

import Data.AffineSpace                         -- package: vector-space



--------------------------------------------------------------------------------
-- Construction


-- | Lift a list of primitives to a composite picture.
--
-- The order of the list maps to the order of printing - the 
-- front of the list is drawn first in the file. This also means
-- that the front of the list is drawn /at the back/ in the 
-- Z-Order.
--
-- This function throws an error when supplied the empty list.
--
frame :: (Real u, Floating u, FromPtSize u) => [PrimElement u] -> Picture u
frame []     = error "Wumpus.Core.Picture.frame - empty list"
frame (p:ps) = let (bb,ones) = step p ps in Leaf (bb,[]) ones 
  where
    step a []     = (boundary a, one a)
    step a (x:xs) = let (bb', rest) = step x xs
                    in ( boundary a `boundaryUnion` bb', cons a rest )



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
                    in ( boundary a `boundaryUnion` bb', cons a rest )


-- | Update the font /delta/ attributes for SVG output.
--
-- Note - 'fontDeltaContext' does not set the font properties of
-- elements in the supplied Picture, it is solely a mechanism to
-- help reduce the code size of the generated SVG by factoring 
-- common attributes into a group (g) element. For instance, 
-- settting the font properties with 'fontDeltaContext' can 
-- eliminate the repeated use of font-family and font-size in 
-- this code:
--
-- > <text font-family="Helvetica" font-size="12"> ... </text>
-- > <text font-family="Helvetica" font-size="12"> ... </text>
-- > <text font-family="Helvetica" font-size="12"> ... </text>
--
-- With the appropriate font delta context, this code will be
-- generated:
-- 
-- > <g font-family="Helvetica" font-size="12">
-- >   <text > ... </text>
-- >   <text > ... </text>
-- >   <text > ... </text>
-- > </g>
--
-- Wumpus ignores 'fontDeltaContext' directives when generating 
-- PostScript. Unlike SVG, PostScript is not naturally nested, so 
-- introducing nesting with @gsave@ and @grestore@ is not likely
-- to improve the PostScript Wumpus generates.
--
fontDeltaContext :: FontAttr -> Picture u -> Picture u
fontDeltaContext fa p = Group (boundary p, []) (FontCtx fa) p


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


-- | Create a hyperlink for SVG output.
--
xlinkhref :: String -> XLink
xlinkhref = XLink


-- | Create a hyperlinked group of Primitives.
--
xlinkGroup :: XLink -> [PrimElement u] -> PrimElement u
xlinkGroup _     [] = error "Picture.xlinkGroup - empty prims list"
xlinkGroup xlink (x:xs) = XLinkGroup xlink (step x xs)
  where
    step a []     = one a
    step a (y:ys) = cons a (step y ys) 

--------------------------------------------------------------------------------
-- Take Paths to Primitives

-- *** Stroke

-- | Create a open, stroked path.
--
ostroke :: Num u 
        => RGBi -> StrokeAttr -> PrimPath u -> PrimElement u
ostroke rgb sa p = Atom $ PPath (OStroke sa rgb) p


-- | Create a closed, stroked path.
--
cstroke :: Num u 
        => RGBi -> StrokeAttr -> PrimPath u -> PrimElement u
cstroke rgb sa p = Atom $ PPath (CStroke sa rgb) p


-- | Create an open, stroked path using the default stroke 
-- attributes and coloured black.
--
zostroke :: Num u => PrimPath u -> PrimElement u
zostroke = ostroke black default_stroke_attr
 
-- | Create a closed stroked path using the default stroke 
-- attributes and coloured black.
--
zcstroke :: Num u => PrimPath u -> PrimElement u
zcstroke = cstroke black default_stroke_attr

--------------------------------------------------------------------------------
-- *** Fill


-- | Create a filled path.
--
fill :: Num u => RGBi -> PrimPath u -> PrimElement u
fill rgb p = Atom $ PPath (CFill rgb) p

-- | Create a filled path coloured black. 
zfill :: Num u => PrimPath u -> PrimElement u
zfill = fill black


--------------------------------------------------------------------------------
-- Filled and stroked (closed) paths


-- | Create a closed path that is both filled and stroked (the fill
-- is below in the zorder).
--
-- > fill colour * stroke attrs * stroke_colour * ...
--
fillStroke :: Num u 
        => RGBi -> StrokeAttr -> RGBi -> PrimPath u -> PrimElement u
fillStroke frgb sa srgb p = Atom $ PPath (CFillStroke frgb sa srgb) p




--------------------------------------------------------------------------------
-- Clipping 

-- | Clip a picture with respect to the supplied path.
--
clip :: (Num u, Ord u) => PrimPath u -> Picture u -> Picture u
clip cp p = Clip (pathBoundary cp, []) cp p

--------------------------------------------------------------------------------
-- Labels to primitive

-- | Create a text label. The string should not contain newline
-- or tab characters. Also double-spaces should not be used - a 
-- rendering agent for SVG will coalesce double-spaces into a 
-- single space. For precise control of spacing and kerning use
-- 'hkernlabel'.
--
-- The supplied point is the left baseline.
--
textlabel :: Num u 
          => RGBi -> FontAttr -> String -> Point2 u -> PrimElement u
textlabel rgb attr txt pt = Atom $ PLabel (LabelProps rgb attr) lbl 
  where
    lbl = PrimLabel pt (StdLayout $ lexLabel txt) identityCTM


-- | Create a label where the font is @Courier@, text size is 14pt
-- and colour is black.
--
ztextlabel :: Num u => String -> Point2 u -> PrimElement u
ztextlabel = textlabel black wumpus_default_font


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
                    , font_enc_name     = latin1_font_encoder
                    }

--------------------------------------------------------------------------------

-- | Create a text label with horizontal /kerning/ for each 
-- character. 
--
-- Note - kerning is relative to the left baseline of the 
-- previous character, it is \*not relative\* to the right-hand
-- boundary of the previous char. While the later would be more
-- obvious it would take a lot of effort to implement as it would 
-- need access to the metrics encoded in font files. 
--
-- Characters are expected to be drawn left to right, so 
-- displacements should not be negative. If the displacement is
-- zero the character will be drawn ontop of the previous char.
-- 
-- The charcters should not contain newline or tab characters.
--
-- The supplied point is the left baseline.
--
-- \*\* CAUTION \*\* - @hkernlabel@ generates a coordinate list 
-- for X-positions rather than a single start point. This is 
-- perfectly valid SVG, but it is not universally supported by 
-- renderers. Chrome support is fine, but Firefox and Safari 
-- currently seem lacking. 
--
hkernlabel :: Num u 
            => RGBi -> FontAttr -> [KerningChar u] -> Point2 u 
            -> PrimElement u
hkernlabel rgb attr xs pt = Atom $ PLabel (LabelProps rgb attr) lbl 
  where
    lbl = PrimLabel pt (KernTextH xs) identityCTM



-- | Create a text label with vertical /kerning/ for each 
-- character - the text is expected to grow downwards. 
--
-- Note - /kerning/ here is the measure between baselines of 
-- sucessive characters, it is \*not\* the distance between the 
-- bottom of one chararter and the top of the next character.
-- 
-- While the later maybe be more obvious from a drawing 
-- perspective, it would take a lot of effort to implement as it 
-- would need access to the metrics encoded in font files. 
--
-- Characters are expected to be drawn downwards - a positive 
-- number represents the downward displacement - so displacements 
-- should not be negative. If the displacement is zero the 
-- character will be drawn ontop of the previous char.
-- 
-- The charcters should not contain newline or tab characters.
--
-- The supplied point is the left baseline of the top character.
--
-- \*\* CAUTION \*\* - @vkernlabel@ generates a coordinate list 
-- for Y-positions rather than a single start point. This is 
-- perfectly valid SVG, but it is not universally supported by 
-- renderers. Chrome support is fine, but Firefox and Safari 
-- currently seem lacking. 
--
vkernlabel :: Num u 
            => RGBi -> FontAttr -> [KerningChar u] -> Point2 u 
            -> PrimElement u
vkernlabel rgb attr xs pt = Atom $ PLabel (LabelProps rgb attr) lbl 
  where
    lbl = PrimLabel pt (KernTextV xs) identityCTM



-- | Construct a regular (i.e. non-special) Char along with its 
-- displacement from the left-baseline of the previous Char.
--
kernchar :: u -> Char -> KerningChar u
kernchar u c = (u, CharLiteral c)

-- | Construct a Char by its character code along with its 
-- displacement from the left-baseline of the previous Char.
--
kernEscInt :: u -> Int -> KerningChar u
kernEscInt u i = (u, CharEscInt i)

-- | Construct a Char by its character name along with its 
-- displacement from the left-baseline of the previous Char.
--
kernEscName :: u -> String -> KerningChar u
kernEscName u s = (u, CharEscName s)

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
             => RGBi -> StrokeAttr -> u -> u -> Point2 u -> PrimElement u
strokeEllipse rgb sa hw hh pt = 
    Atom $ PEllipse (EStroke sa rgb) (PrimEllipse pt hw hh identityCTM)

-- | Create a filled ellipse.
--
fillEllipse :: Num u 
             => RGBi -> u -> u -> Point2 u -> PrimElement u
fillEllipse rgb hw hh pt = 
    Atom $ PEllipse (EFill rgb) (PrimEllipse pt hw hh identityCTM)


-- | Create a black, filled ellipse. 
zellipse :: Num u => u -> u -> Point2 u -> PrimElement u
zellipse hw hh pt = fillEllipse black hw hh pt


-- | Create a bordered (i.e. filled and stroked) ellipse.
--
fillStrokeEllipse :: Num u 
                 => RGBi -> StrokeAttr -> RGBi -> u -> u -> Point2 u 
                 -> PrimElement u
fillStrokeEllipse frgb sa srgb hw hh pt = 
    Atom $ PEllipse (EFillStroke frgb sa srgb) (PrimEllipse pt hw hh identityCTM)



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
a `picOver` b = Picture (bb,[]) (cons b $ one a) 
  where
    bb = boundary a `boundaryUnion` boundary b

-- picOver note - draw b, put b first in the list, so it draws 
-- first in the output (this is also @behind@ in the Z-Order).


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
                     => RGBi -> PrimElement u -> Picture u
illustrateBoundsPrim rgb p = frame (p : boundsPrims rgb p)



-- | Draw a the rectangle of a bounding box, plus cross lines
-- joining the corners.
--
boundsPrims :: (Num u, Ord u, Boundary t, u ~ DUnit t) 
            => RGBi -> t -> [PrimElement u]
boundsPrims rgb a = [ bbox_rect, bl_to_tr, br_to_tl ]
  where
    (bl,br,tr,tl) = boundaryCorners $ boundary a
    bbox_rect     = cstroke rgb line_attr $ vertexPath [bl,br,tr,tl]
    bl_to_tr      = ostroke rgb line_attr $ vertexPath [bl,tr]
    br_to_tl      = ostroke rgb line_attr $ vertexPath [br,tl]

    line_attr     = default_stroke_attr { line_cap     = CapRound
                                        , dash_pattern = Dash 0 [(1,2)] }


-- | Generate the control points illustrating the Bezier curves 
-- within a picture.
-- 
-- This has no effect on TextLabels. Nor does it draw Beziers of 
-- a hyperlinked obkect.
-- 
-- Pseudo control points are generated for ellipses, although 
-- strictly speaking ellipses do not use Bezier curves - they 
-- are implemented with PostScript\'s @arc@ command.  
--
illustrateControlPoints :: (Real u, Floating u, FromPtSize u)
                        => RGBi -> PrimElement u -> Picture u
illustrateControlPoints rgb elt = outer elt
  where
    outer a@(Atom prim) = frame (a : step prim)
    outer a             = frame [a]

    step (PEllipse _ e) = ellipseCtrlLines rgb e
    step (PPath    _ p) = pathCtrlLines rgb p
    step _              = []

-- Genrate lines illustrating the control points of curves on 
-- a Path.
--
-- Two lines are generated for a Bezier curve:
-- start-point to control-point1; control-point2 to end-point
--
-- Nothing is generated for a straight line.
--
pathCtrlLines :: (Num u, Ord u) => RGBi -> PrimPath u -> [PrimElement u]
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
                 => RGBi -> PrimEllipse u -> [PrimElement u]
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

