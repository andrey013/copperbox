{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.Picture
-- Copyright   :  (c) Stephen Tetley 2009-2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Construction of pictures, paths and text labels.
--
-- Note - the text label functions are only appropriate for single 
-- printed lines of text. Wumpus-Core does not interpret special 
-- white-space characters (i.e. tab or newline) and passes them 
-- forward into the output file. Renderers will then deal with 
-- white-space as they please, which is usually to ignore it.
--
-- Also, SVG output does not currently use @space=\"preserve\"@. 
-- It would be desirable to do so, as the PostScript generated by 
-- Wumpus can use doubled up spaces to print wide spaces. 
-- Unfortunately, SVG renderers seem to differ as to whether they
-- can inherit @space=\"preserve\"@ from a top level attribute or
-- whether it must be annotated on every @\<text\>@ element. 
-- Wumpus considers the latter to be a too-high burden as it will
-- expand the code size (this view may change).
--
--------------------------------------------------------------------------------


module Wumpus.Core.Picture
  ( 

  -- * Construction
    frame
  , multi
  , fontDeltaContext
  , primPath
  , lineTo
  , curveTo
  , vertexPath
  , vectorPath
  , emptyPath
  , curvedPath
  , xlinkhref
  , xlink
  , svgattr
  , annotateGroup
  , annotateXLink

  , primGroup
  , primCat


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
  , rtextlabel
  , ztextlabel

  , escapedlabel
  , rescapedlabel
  , zescapedlabel

  , hkernlabel
  , vkernlabel
  , kernchar
  , kernEscInt
  , kernEscName

  , strokeEllipse
  , rstrokeEllipse
  , fillEllipse
  , rfillEllipse
  , zellipse
  , fillStrokeEllipse
  , rfillStrokeEllipse  

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
import Wumpus.Core.GraphicProps
import Wumpus.Core.PictureInternal
import Wumpus.Core.Text.Base
import Wumpus.Core.TrafoInternal
import Wumpus.Core.Units
import Wumpus.Core.Utils.FormatCombinators hiding ( fill )
import Wumpus.Core.Utils.HList
import Wumpus.Core.Utils.JoinList

import Data.AffineSpace                         -- package: vector-space
import Data.VectorSpace

import Data.List ( mapAccumL )

--------------------------------------------------------------------------------
-- Construction


-- | Lift a list of primitives to a composite picture.
--
-- The order of the list maps to the order of printing - the 
-- front of the list is drawn first in the file. This also means
-- that the front of the list is drawn /at the back/ in the 
-- Z-Order.
--
-- \*\* WARNING \*\* - this function throws a runtime error when 
-- supplied the empty list.
--
frame :: (Real u, Floating u, PtSize u) => [Primitive u] -> Picture u
frame []     = error "Wumpus.Core.Picture.frame - empty list"
frame (p:ps) = let (bb,ones) = step p ps in Leaf (bb,[]) ones 
  where
    step a []     = (boundary a, one a)
    step a (x:xs) = let (bb', rest) = step x xs
                    in ( boundary a `boundaryUnion` bb', cons a rest )



-- | Place multiple pictures within the standard affine frame.
--
-- \*\* WARNING \*\* - this function throws a runtime error when 
-- supplied the empty list.
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
fontDeltaContext :: FontAttr -> Primitive u -> Primitive u
fontDeltaContext fa p = PContext (FontCtx fa) p


-- | 'primPath' : @ start_point * [path_segment] -> PrimPath @
--
-- Create a Path from a start point and a list of PathSegments.
--
primPath :: Num u => Point2 u -> [AbsPathSegment u] -> PrimPath u
primPath pt xs = PrimPath pt $ step pt xs
  where
    step p (AbsLineTo p1:rest)        = RelLineTo (p1 .-. p) : step p1 rest
    step p (AbsCurveTo p1 p2 p3:rest) = 
        RelCurveTo (p1 .-. p) (p2 .-. p1) (p3 .-. p2) : step p3 rest

    step _ []                         = []


         
-- | 'lineTo' : @ end_point -> path_segment @
-- 
-- Create a straight-line PathSegment, the start point is 
-- implicitly the previous point in a path.
--
lineTo :: Point2 u -> AbsPathSegment u
lineTo = AbsLineTo

-- | 'curveTo' : @ control_point1 * control_point2 * end_point -> 
--        path_segment @
-- 
-- Create a curved PathSegment, the start point is implicitly the 
-- previous point in a path.
--
--
curveTo :: Point2 u -> Point2 u -> Point2 u -> AbsPathSegment u
curveTo = AbsCurveTo


-- | 'vertexPath' : @ [point] -> PrimPath @
-- 
-- Convert the list of vertices to a path of straight line 
-- segments.
--
-- \*\* WARNING \*\* - this function throws a runtime error when 
-- supplied the empty list.
--
vertexPath :: Num u => [Point2 u] -> PrimPath u
vertexPath []     = error "Picture.vertexPath - empty point list"
vertexPath (x:xs) = PrimPath x $ snd $ mapAccumL step x xs
  where
    step a b = let v = b .-. a in (b, RelLineTo v)


-- | 'vectorPath' : @ start_point -> [next_vector] -> PrimPath @
-- 
-- Build a \"relative\" path from the start point, appending 
-- successive straight line segments formed from the list of 
-- next_vectors.
-- 
-- This function can be supplied with an empty list - this 
-- simulates a null graphic.
--
vectorPath :: Num u => Point2 u -> [Vec2 u] -> PrimPath u
vectorPath pt xs = PrimPath pt $ map RelLineTo xs


-- | 'emptyPath' : @ start_point -> PrimPath @
-- 
-- Build an empty path. The start point must be specified even
-- though the path is not drawn - a start point is the minimum 
-- information needed to calculate a bounding box. 
--
emptyPath :: Num u => Point2 u -> PrimPath u
emptyPath pt  = PrimPath pt []



-- | 'curvedPath' : @ points -> PrimPath @
-- 
-- Convert a list of vertices to a path of curve segments.
-- The first point in the list makes the start point, each curve 
-- segment thereafter takes 3 points. /Spare/ points at the end 
-- are discarded. 
--
-- \*\* WARNING - this function throws an error when supplied the 
-- empty list.
-- 
curvedPath :: Num u => [Point2 u] -> PrimPath u
curvedPath []     = error "Picture.curvedPath - empty point list"
curvedPath (x:xs) = PrimPath x $ step x xs
  where
    step p (a:b:c:ys) = let v1 = a .-. p 
                            v2 = b .-. a
                            v3 = c .-. b
                        in RelCurveTo v1 v2 v3 : step c ys
    step _ _          = []

-- | Create a hyperlink for SVG output.
--
xlinkhref :: String -> XLink
xlinkhref = XLink


-- | Create a hyperlinked Primitive.
--
xlink :: XLink -> Primitive u -> Primitive u
xlink hypl p = PSVG (ALink hypl) p  


-- | Create an attribute for SVG output.
--
-- Attributes are expected to be /non-graphical/ e.g. @onclick@ 
-- events or similar. Wumpus does not check the syntax and simply
-- emits the Strings as-is in the output. 
--
-- Graphical properties should not be encoded, they may conflict 
-- with output that Wumpus produces.
--
-- \*\* WARNING \*\* - currently this functionality is 
-- undercooked. Because SVG has more /extra-graphical/ facilities
-- than PostScript (hyperlinks, mouseovers, etc.) it seems 
-- important to have an escape hatch to them, yet so far the 
-- escape hatch has not been needed.
--
svgattr :: String -> String -> SvgAttr
svgattr = SvgAttr


-- | Add SVG attribute annotations to a Primitive.
--
-- The primitive will be printed in a @g@ (group) element 
-- labelled with the annotations.
--
annotateGroup :: [SvgAttr] -> Primitive u -> Primitive u
annotateGroup xs p = PSVG (GAnno xs) p  


-- | Add SVG XLink and attribute annotations to a Primitive.
--
-- The primitive will be printed in a @g@ (group) element, itself
-- inside an @a@ link.
--
annotateXLink :: XLink -> [SvgAttr] -> Primitive u -> Primitive u
annotateXLink hypl xs p = PSVG (SvgAG hypl xs) p  



-- | Group a list of Primitives.
--
-- \*\* WARNING \*\* - this function throws a runtime error when 
-- supplied the empty list.
--
primGroup :: [Primitive u] -> Primitive u
primGroup []     = error "Picture.primGroup - empty prims list"
primGroup (x:xs) = PGroup (step x xs)
  where
    step a []     = one a
    step a (y:ys) = cons a (step y ys) 


-- | Concatenate two Primitives.
--
-- If both primitves are groups, then the groups are merged.
--
-- If one or other primitive is a group, the singleton is added
-- into the group at the respective end.
--
-- Otherwise a group is formed adding both elements as /children/.
--
-- The Primitive type in Wumpus is a tree. In theory 'primCat' 
-- can make flatter and wider trees than 'primGroup', though in 
-- practice this may have no noticeable benefit as Wumpus has very 
-- simple access patterns into the Primitive tree. 
--
primCat :: Primitive u -> Primitive u -> Primitive u
primCat (PGroup a) (PGroup b) = PGroup $ join a b
primCat (PGroup a) prim       = PGroup $ join a (one prim) 
primCat prim       (PGroup b) = PGroup $ join (one prim) b
primCat p1         p2         = PGroup $ join (one p1) (one p2) 



--------------------------------------------------------------------------------
-- Take Paths to Primitives

-- *** Stroke

-- | 'ostroke' : @ rgb * stroke_attr * path -> Primitive @
--
-- Create a open, stroked path.
--
ostroke :: Num u 
        => RGBi -> StrokeAttr -> PrimPath u -> Primitive u
ostroke rgb sa p = PPath (OStroke sa rgb) p


-- | 'cstroke' : @ rgb * stroke_attr * path -> Primitive @
-- 
-- Create a closed, stroked path.
--
cstroke :: Num u 
        => RGBi -> StrokeAttr -> PrimPath u -> Primitive u
cstroke rgb sa p = PPath (CStroke sa rgb) p


-- | 'zostroke' : @ path -> Primitive @
--
-- Create an open, stroked path using the default stroke 
-- attributes and coloured black.
--
zostroke :: Num u => PrimPath u -> Primitive u
zostroke = ostroke black default_stroke_attr
 
-- | 'zcstroke' : @ path -> Primitive @
--
-- Create a closed stroked path using the default stroke 
-- attributes and coloured black.
--
zcstroke :: Num u => PrimPath u -> Primitive u
zcstroke = cstroke black default_stroke_attr

--------------------------------------------------------------------------------
-- *** Fill


-- | 'fill' : @ rgb * path -> Primitive @
--
--  Create a filled path.
--
fill :: Num u => RGBi -> PrimPath u -> Primitive u
fill rgb p = PPath (CFill rgb) p

-- | 'zfill' : @ path -> Primitive @
--
-- Create a filled path coloured black. 
zfill :: Num u => PrimPath u -> Primitive u
zfill = fill black


--------------------------------------------------------------------------------
-- Filled and stroked (closed) paths


-- | 'fillStroke' : @ fill_rgb * stroke_attr * stroke_rgb * path -> Primitive @
--
-- Create a closed path that is both filled and stroked (the fill
-- is below in the zorder).
--
fillStroke :: Num u 
        => RGBi -> StrokeAttr -> RGBi -> PrimPath u -> Primitive u
fillStroke frgb sa srgb p = PPath (CFillStroke frgb sa srgb) p




--------------------------------------------------------------------------------
-- Clipping 

-- | 'clip' : @ path * primitive -> Primitive @
-- 
-- Clip a primitive with respect to the supplied path.
--
clip :: (Num u, Ord u) => PrimPath u -> Primitive u -> Primitive u
clip cp p = PClip cp p

--------------------------------------------------------------------------------
-- Labels to primitive

-- | 'textlabel' : @ rgb * font_attr * string * baseline_left -> Primitive @
--
-- Create a text label. The string should not contain newline
-- or tab characters. Also double-spaces should not be used - a 
-- rendering agent for SVG will coalesce double-spaces into a 
-- single space. For precise control of spacing and kerning use
-- 'hkernlabel'.
--
-- The supplied point is the left baseline.
--
textlabel :: PtSize u 
          => RGBi -> FontAttr -> String -> Point2 u -> Primitive u
textlabel rgb attr txt pt = rtextlabel rgb attr txt 0 pt

-- | 'rtextlabel' : @ rgb * font_attr * string * theta * 
--      baseline_left -> Primitive @
--
-- Create a text label rotated by the supplied angle about the 
-- baseline-left. 
--
-- The supplied point is the left baseline.
--
rtextlabel :: PtSize u 
           => RGBi -> FontAttr -> String -> Radian -> Point2 u -> Primitive u
rtextlabel rgb attr txt pt theta = 
    rescapedlabel rgb attr (escapeString txt) pt theta


-- | 'ztextlabel' : @ string * baseline_left -> Primitive @
--
-- Create a label where the font is @Courier@, text size is 14pt
-- and colour is black.
--
ztextlabel :: PtSize u => String -> Point2 u -> Primitive u
ztextlabel = textlabel black wumpus_default_font



-- | 'escapedlabel' : @ rgb * font_attr * escaped_text * 
--      baseline_left -> Primitive @
--
-- Version of 'textlabel' where the label text has already been 
-- parsed for special characters.
--
-- The supplied point is the left baseline.
--
escapedlabel :: PtSize u 
             => RGBi -> FontAttr -> EscapedText -> Point2 u -> Primitive u
escapedlabel rgb attr txt pt = rescapedlabel rgb attr txt 0 pt

-- | 'rescapedlabel' : @ rgb * font_attr * escaped_text * theta * 
--      baseline_left -> Primitive @
--
-- Version of 'rtextlabel' where the label text has already been 
-- parsed for special characters.
--
-- The supplied point is the left baseline.
--
rescapedlabel :: PtSize u 
              => RGBi -> FontAttr -> EscapedText -> Radian -> Point2 u 
              -> Primitive u
rescapedlabel rgb attr txt theta (P2 dx dy) = PLabel (LabelProps rgb attr) lbl 
  where
    lbl = PrimLabel (StdLayout txt) 
                    (makeThetaCTM (psDouble dx) (psDouble dy) theta)


-- | 'zescapedlabel' : @ escaped_text * baseline_left -> Primitive @
--
-- Version of 'ztextlabel' where the label text has already been 
-- encoded.
--
zescapedlabel :: PtSize u => EscapedText -> Point2 u -> Primitive u
zescapedlabel = escapedlabel black wumpus_default_font



--------------------------------------------------------------------------------

-- | 'hkernlabel' : @ rgb * font_attr * kerning_chars * 
--        baseline_left -> Primitive @
--
-- Create a text label with horizontal /kerning/ for each 
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
-- \*\* CAUTION \*\* - for SVG, @hkernlabel@ generates a 
-- coordinate list of X-positions rather than a single start 
-- point. This is syntactically valid SVG, but it is not 
-- universally supported by renderers. Chrome support is fine, 
-- but Firefox and Safari currently seem lacking. 
--
-- Also, note this feature does not have a directly compatible 
-- PostScript analogue. While the same picture is generated in 
-- both cases, the PostScript code is not particularly inefficient.
--
hkernlabel :: PtSize u 
           => RGBi -> FontAttr -> [KerningChar u] -> Point2 u 
           -> Primitive u
hkernlabel rgb attr xs (P2 x y) = PLabel (LabelProps rgb attr) lbl 
  where
    lbl = PrimLabel (KernTextH xs) (makeTranslCTM (psDouble x) (psDouble y))



-- | 'vkernlabel' : @ rgb * font_attr * kerning_chars * 
--        baseline_left -> Primitive @
--
-- Create a text label with vertical /kerning/ for each 
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
-- \*\* CAUTION \*\* - for SVG, @hkernlabel@ generates a 
-- coordinate list of Y-positions rather than a single start 
-- point. This is syntactically valid SVG, but it is not 
-- universally supported by renderers. Chrome support is fine, 
-- but Firefox and Safari currently seem lacking. 
--
-- Also, note this feature does not have a directly compatible 
-- PostScript analogue. While the same picture is generated in 
-- both cases, the PostScript code is not particularly inefficient.
--
vkernlabel :: PtSize u 
           => RGBi -> FontAttr -> [KerningChar u] -> Point2 u 
           -> Primitive u
vkernlabel rgb attr xs (P2 x y) = PLabel (LabelProps rgb attr) lbl 
  where
    lbl = PrimLabel (KernTextV xs) (makeTranslCTM (psDouble x) (psDouble y))



-- | 'kernchar' : @ displacement * char -> KerningChar @
-- 
-- Construct a regular (i.e. non-special) Char along with its 
-- displacement from the left-baseline of the previous Char.
--
kernchar :: u -> Char -> KerningChar u
kernchar u c = (u, CharLiteral c)


-- | 'kernEscInt' : @ displacement * char_code -> KerningChar @
-- 
-- Construct a Char by its character code along with its 
-- displacement from the left-baseline of the previous Char.
--
kernEscInt :: u -> Int -> KerningChar u
kernEscInt u i = (u, CharEscInt i)


-- | 'kernEscName' : @ displacement * char_name -> KerningChar @
-- 
-- Construct a Char by its character name along with its 
-- displacement from the left-baseline of the previous Char.
--
kernEscName :: u -> String -> KerningChar u
kernEscName u s = (u, CharEscName s)

--------------------------------------------------------------------------------


-- | 'strokeEllipse' : @ rgb * stroke_attr * rx * ry * center -> Primtive @
-- 
-- Create a stroked ellipse.
--
-- Note - within Wumpus, ellipses are considered an unfortunate
-- but useful /optimization/. Drawing good cicles with Beziers 
-- needs four curves, but drawing them with PostScript\'s @arc@ 
-- command uses a single operation. For drawings with many dots 
-- (e.g. scatter plots) it seems sensible to employ this 
-- optimization.
--
-- A deficiency of using PostScript\'s @arc@ command to draw
-- ellipses is that (non-uniformly) scaling a stroked ellipse 
-- also (non-uniformly) scales the pen it is drawn with. Where 
-- the ellipse is wider, the pen stroke 
-- will be wider too. 
--
-- Avoid non-uniform scaling stroked ellipses!
--
strokeEllipse :: PtSize u 
              => RGBi -> StrokeAttr -> u -> u -> Point2 u -> Primitive u
strokeEllipse rgb sa hw hh pt = rstrokeEllipse rgb sa hw hh 0 pt


-- | 'rstrokeEllipse' : @ rgb * stroke_attr * rx * ry * theta * 
--      center -> Primtive @
-- 
-- Create a stroked primitive ellipse rotated about the center by 
-- /theta/.
--
rstrokeEllipse :: PtSize u 
               => RGBi -> StrokeAttr -> u -> u -> Radian -> Point2 u
               -> Primitive u
rstrokeEllipse rgb sa rx ry theta pt = 
    PEllipse (EStroke sa rgb) (mkPrimEllipse rx ry theta pt)



-- | 'fillEllipse' : @ rgb * rx * ry * center -> Primtive @
--
-- Create a filled primitive ellipse.
--
fillEllipse :: PtSize u 
            => RGBi -> u -> u -> Point2 u -> Primitive u
fillEllipse rgb rx ry pt = rfillEllipse rgb rx ry 0 pt
 

-- | 'rfillEllipse' : @ rgb * rx * ry * theta * center -> Primitive @
--
-- Create a filled primitive ellipse rotated about the center by 
-- /theta/.
--
rfillEllipse :: PtSize u 
             => RGBi -> u -> u -> Radian -> Point2 u -> Primitive u
rfillEllipse rgb rx ry theta pt = 
    PEllipse (EFill rgb) (mkPrimEllipse rx ry theta pt)


-- | 'zellipse' : @ rx * ry * center -> Primtive @
--
-- Create a black, filled ellipse. 
--
zellipse :: PtSize u => u -> u -> Point2 u -> Primitive u
zellipse hw hh pt = rfillEllipse black hw hh 0 pt


-- | 'fillStrokeEllipse' : @ fill_rgb * stroke_attr * stroke_rgb * rx * ry *
--      center -> Primtive @
--
-- Create a bordered (i.e. filled and stroked) primitive ellipse.
--
fillStrokeEllipse :: PtSize u 
                  => RGBi -> StrokeAttr -> RGBi -> u -> u -> Point2 u 
                  -> Primitive u
fillStrokeEllipse frgb sa srgb rx ry pt = 
    rfillStrokeEllipse frgb sa srgb rx ry 0 pt
    


-- | 'rfillStrokeEllipse' : @ fill_rgb * stroke_attr * stroke_rgb * rx * ry *
--      theta * center -> Primtive @
--
-- Create a bordered (i.e. filled and stroked) ellipse rotated 
-- about the center by /theta/.
--
rfillStrokeEllipse :: PtSize u 
                   => RGBi -> StrokeAttr -> RGBi -> u -> u -> Radian -> Point2 u
                   -> Primitive u
rfillStrokeEllipse frgb sa srgb rx ry theta pt = 
    PEllipse (EFillStroke frgb sa srgb) (mkPrimEllipse rx ry theta pt)


mkPrimEllipse :: PtSize u => u -> u -> Radian -> Point2 u -> PrimEllipse u
mkPrimEllipse rx ry theta (P2 dx dy) = 
    PrimEllipse rx ry (makeThetaCTM (psDouble dx) (psDouble dy) theta)

--------------------------------------------------------------------------------
-- Operations

-- | 'extendBoundary' : @ x * y * picture -> Picture @
-- 
-- Extend the bounding box of a picture. 
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

-- | 'picOver' : @ picture * picture -> Picture @
--
-- Draw the first picture on top of the second picture - 
-- neither picture will be moved.
--
picOver :: (Num u, Ord u) => Picture u -> Picture u -> Picture u
a `picOver` b = Picture (bb,[]) (join (one b) (one a))   
  where
    bb = boundary a `boundaryUnion` boundary b

-- NOTE - picOver - draw b, put b first in the list, so it draws 
-- first in the output (this is also @behind@ in the Z-Order).


-- | 'picMoveBy' : @ picture * vector -> Picture @
-- 
--  Move a picture by the supplied vector. 
--
picMoveBy :: (PtSize u, Ord u) => Picture u -> Vec2 u -> Picture u
p `picMoveBy` (V2 dx dy) = translate dx dy p 

-- | 'picBeside' : @ picture * picture -> Picture @
--
-- Move the second picture to sit at the right side of the
-- first picture
--
picBeside :: (PtSize u, Ord u) => Picture u -> Picture u -> Picture u
a `picBeside` b = a `picOver` (b `picMoveBy` v) 
  where 
    (P2 x1 _) = ur_corner $ boundary a
    (P2 x2 _) = ll_corner $ boundary b 
    v         = hvec $ x1 - x2 

--------------------------------------------------------------------------------
-- Illustrating pictures and primitives

-- | Print the syntax tree of a Picture to the console.
--
printPicture :: (Num u, Format u) => Picture u -> IO ()
printPicture pic = putStrLn (show $ format pic) >> putStrLn []


-- | 'illustrateBounds' : @ bbox_rgb * picture -> Picture @
-- 
-- Draw the picture on top of an image of its bounding box.
-- The bounding box image will be drawn in the supplied colour.
--
illustrateBounds :: (Real u, Floating u, PtSize u) 
                 => RGBi -> Picture u -> Picture u
illustrateBounds rgb p = p `picOver` (frame $ boundsPrims rgb p $ []) 


-- | 'illustrateBoundsPrim' : @ bbox_rgb * primitive -> Picture @
-- 
-- Draw the primitive on top of an image of its bounding box.
-- The bounding box image will be drawn in the supplied colour.
--
-- The result will be lifted from Primitive to Picture.
-- 
illustrateBoundsPrim :: (Real u, Floating u, PtSize u) 
                     => RGBi -> Primitive u -> Picture u
illustrateBoundsPrim rgb p = frame $ boundsPrims rgb p $ [p]



-- | Draw a the rectangle of a bounding box, plus cross lines
-- joining the corners.
--
boundsPrims :: (Num u, Ord u, Boundary t, u ~ DUnit t) 
            => RGBi -> t -> H (Primitive u)
boundsPrims rgb a = fromListH $ [ bbox_rect, bl_to_tr, br_to_tl ]
  where
    (bl,br,tr,tl) = boundaryCorners $ boundary a
    bbox_rect     = cstroke rgb line_attr $ vertexPath [bl,br,tr,tl]
    bl_to_tr      = ostroke rgb line_attr $ vertexPath [bl,tr]
    br_to_tl      = ostroke rgb line_attr $ vertexPath [br,tl]

    line_attr     = default_stroke_attr { line_cap     = CapRound
                                        , dash_pattern = Dash 0 [(1,2)] }


-- | 'illustrateControlPoints' : @ control_point_rgb * primitive -> Picture @
-- 
-- Generate the control points illustrating the Bezier curves 
-- within a picture.
-- 
-- This has no effect on TextLabels. Nor does it draw Beziers of 
-- a hyperlinked object.
-- 
illustrateControlPoints :: (Real u, Floating u, PtSize u)
                        => RGBi -> Primitive u -> Picture u
illustrateControlPoints rgb elt = frame $ fn elt
  where
    fn (PPath    _ p) = pathCtrlLines rgb p $ [elt]
    fn a              = [a]


-- Genrate lines illustrating the control points of curves on 
-- a Path.
--
-- Two lines are generated for a Bezier curve:
-- start-point to control-point1; control-point2 to end-point
--
-- Nothing is generated for a straight line.
--
pathCtrlLines :: (Num u, Ord u) => RGBi -> PrimPath u -> H (Primitive u)
pathCtrlLines rgb (PrimPath start ss) = step start ss
  where 
    step s (RelLineTo v:xs)         = step (s .+^ v) xs

    step s (RelCurveTo v1 v2 v3:xs) = let e   = s .+^ (v1 ^+^ v2 ^+^ v3)
                                          v3r = vreverse v3
                                      in mkLine s v1 `consH` mkLine e v3r 
                                                     `consH` step e xs

    step _ []                       = emptyH

    mkLine s v                      = let pp = (PrimPath s [RelLineTo v]) 
                                      in ostroke rgb default_stroke_attr pp 

