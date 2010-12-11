{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.PictureInternal
-- Copyright   :  (c) Stephen Tetley 2009-2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Internal representation of Pictures.
--
--------------------------------------------------------------------------------


module Wumpus.Core.PictureInternal
  ( 

    Picture(..)
  , DPicture
  , Locale
  , FontCtx(..)

  , Primitive(..)
  , DPrimitive
  , SvgAnno(..)
  , XLink(..)
  , SvgAttr(..)

  , PrimPath(..)
  , DPrimPath
  , PrimPathSegment(..)
  , DPrimPathSegment
  , AbsPathSegment(..)
  , DAbsPathSegment
  , PrimLabel(..)
  , DPrimLabel
  , LabelBody(..)
  , DLabelBody
  , KerningChar
  , DKerningChar  
  , PrimEllipse(..)

  , GraphicsState(..)

  , pathBoundary
  , mapLocale

  -- * Additional operations
  , concatTrafos
  , deconsMatrix
  , repositionDeltas

  , zeroGS
  , isEmptyPath
  , isEmptyLabel

  ) where

import Wumpus.Core.AffineTrans
import Wumpus.Core.BoundingBox
import Wumpus.Core.Colour
import Wumpus.Core.FontSize
import Wumpus.Core.Geometry
import Wumpus.Core.GraphicProps
import Wumpus.Core.PtSize
import Wumpus.Core.Text.Base
import Wumpus.Core.TrafoInternal
import Wumpus.Core.Utils.Common
import Wumpus.Core.Utils.FormatCombinators
import Wumpus.Core.Utils.JoinList


import Data.AffineSpace                         -- package: vector-space

import qualified Data.Foldable                  as F
import qualified Data.IntMap                    as IntMap


-- | Picture is a leaf attributed tree - where attributes are 
-- colour, line-width etc. It is parametric on the unit type 
-- of points (typically Double).
-- 
-- Wumpus\'s leaf attributed tree, is not directly matched to 
-- PostScript\'s picture representation, which might be 
-- considered a node attributed tree (if you consider graphics
-- state changes less imperatively - setting attributes rather 
-- than global state change).
--
-- Considered as a node-attributed tree PostScript precolates 
-- graphics state updates downwards in the tree (vis-a-vis 
-- inherited attributes in an attibute grammar), where a 
-- graphics state change deeper in the tree overrides a higher 
-- one.
-- 
-- Wumpus on the other hand, simply labels each leaf with its
-- drawing attributes - there is no attribute inheritance.
-- When it draws the PostScript picture it does some 
-- optimization to avoid generating excessive graphics state 
-- changes in the PostScript code.
--
-- Omitting some details, Picture is a simple non-empty 
-- leaf-labelled rose tree via:
-- 
-- > tree = Leaf [primitive] | Picture [tree]
--
-- The additional constructors are convenience:
--
-- @Clip@ nests a picture (tree) inside a clipping path.
--
-- The @Group@ constructor allows local shared graphics state 
-- updates for the SVG renderer - in some instances this can 
-- improve the code size of the generated SVG.
--
data Picture u = Leaf     (Locale u)              (JoinList (Primitive u))
               | Picture  (Locale u)              (JoinList (Picture u))
               | Clip     (Locale u) (PrimPath u) (Picture u)
  deriving (Show)

type DPicture = Picture Double



-- | Locale = (bounding box * current translation matrix)
-- 
-- Pictures (and sub-pictures) are located frame consisting of a 
-- bounding box and a translation matrix (represented as a list 
-- of affine transformations). So that pictures can be arranged 
-- via vertical and horizontal composition their bounding box is 
-- cached.
--
-- In Wumpus, affine transformations (scalings, rotations...)
-- transform the CTM rather than the constituent points of 
-- the primitives. Changes of CTM are transmitted to PostScript
-- as @concat@ commands (and matrix transforms in SVG).
--  
-- So that picture composition is remains stable under affine
-- transformation, the corners of bounding boxes are transformed
-- pointwise when the picture is scaled, rotated etc.
--
type Locale u = (BoundingBox u, [AffineTrafo u])



-- | Wumpus\'s drawings are built from two fundamental 
-- primitives: paths (straight line segments and Bezier curves) 
-- and labels (single lines of text). 
-- 
-- Ellipses are a included as a primitive only for optimization 
-- - drawing a reasonable circle with Bezier curves needs at 
-- least eight curves. This is inconvenient for drawing dots 
-- which can otherwise be drawn with a single @arc@ command.
-- 
-- Wumpus does not follow PostScript employing arc as a general 
-- path primitive - arcs are used only to draw ellipses. This 
-- is because arcs do not enjoy the nice properties of Bezier 
-- curves, whereby the affine transformation of a Bezier curve 
-- can simply be achieved by the affine transformation of it\'s 
-- control points.
--
-- Ellipses are represented by their center, half-width and 
-- half-height. Half-width and half-height are used so the 
-- bounding box can be calculated using only multiplication, and 
-- thus initially only obliging a Num constraint on the unit.
-- Though typically for affine transformations a Fractional 
-- constraint is also obliged.
--
-- To represent XLink hyperlinks, Primitives can be annotated 
-- with some a hyperlink (likewise a /passive/ font change for 
-- better SVG code generation) and grouped - a hyperlinked arrow 
-- would want the tip and the arrow body both to be incorporated 
-- in thelink even though they are two drawing primitives. 
--
-- This means that Primitives aren\'t strictly /primitive/ as 
-- the actual implementation is a tree.
-- 
data Primitive u = PPath    PathProps         (PrimPath u)
                 | PLabel   LabelProps        (PrimLabel u)
                 | PEllipse EllipseProps      (PrimEllipse u)
                 | PContext FontCtx           (Primitive u)
                 | PSVG     SvgAnno           (Primitive u)
                 | PGroup   (JoinList (Primitive u))
  deriving (Eq,Show)


type DPrimitive = Primitive Double


-- | Set the font /delta/ for SVG rendering. 
-- 
-- Note - this does not change the default colour or font style. 
-- It is solely a backdoor into the SVG renderer to potential 
-- allow some code size reductions.
--
newtype FontCtx = FontCtx { getFontCtx :: FontAttr }
  deriving (Eq,Show)


-- | SVG annotations - annotations can be: 
-- 
-- A hyperlink inside @<a ...> ... </a>@ .
--
-- A group - @<g ...> ... </g>@
--
-- A group inside a hyperlink.
--
data SvgAnno = ALink XLink
             | GAnno [SvgAttr]
             | SvgAG XLink [SvgAttr]
   deriving (Eq,Show)

-- | Primitives can be grouped with hyperlinks in SVG output.
--
-- Note - this is always printed as @xlink:href="..."@. Other
-- types of xlink can be modelled with the unrestrained 
-- SvgAnno type.
--
newtype XLink = XLink { getXLink :: String }
  deriving (Eq,Show)


-- | Primitives can be labelled with arbitrary SVG properties 
-- (e.g @onmouseover@) within a group element.
--
-- Note - annotations should be used only for non-graphical 
-- properties. Graphical properties (fill_colour, font_size, etc.)
-- should be set through the appropriate Wumpus functions.
--
data SvgAttr = SvgAttr 
      { svg_attr_name   :: String
      , svg_attr_value  :: String 
      }
  deriving (Eq,Show)

-- | PrimPath - start point and a list of path segments.
--
data PrimPath u = PrimPath (Point2 u) [PrimPathSegment u]
  deriving (Eq,Show)

type DPrimPath = PrimPath Double

-- | PrimPathSegment - either a relative cubic Bezier /curve-to/ 
-- or a relative /line-to/.
--
data PrimPathSegment u = RelCurveTo  (Vec2 u) (Vec2 u) (Vec2 u)
                       | RelLineTo   (Vec2 u)
  deriving (Eq,Show)

type DPrimPathSegment = PrimPathSegment Double

-- Design note - if paths were represented as:
--   start-point plus [relative-path-segment]
-- They would be cheaper to move...
--


-- | AbsPathSegment - either a cubic Bezier curve or a line.
-- 
-- Note this data type is transitory - it is only used as a 
-- convenience to build relative paths. 
--
data AbsPathSegment u = AbsCurveTo  (Point2 u) (Point2 u) (Point2 u)
                      | AbsLineTo   (Point2 u)
  deriving (Eq,Show)

type DAbsPathSegment = AbsPathSegment Double



-- | Label - represented by baseline-left point and text.
--
-- Baseline-left is the dx * dy of the PrimCTM.
--
data PrimLabel u = PrimLabel 
      { label_body          :: LabelBody u
      , label_ctm           :: PrimCTM u
      }
  deriving (Eq,Show)

type DPrimLabel = PrimLabel Double


-- | Label can be draw with 3 layouts.
-- 
-- The standard layout uses @show@ for PostScript and a single 
-- initial point for SVG.
--
-- Kerned horizontal layout - each character is encoded with the
-- rightwards horizontal distance from the last charcaters left 
-- base-line.
-- 
-- Kerned vertical layout - each character is encoded with the
-- upwards distance from the last charcaters left base-line.
-- 
data LabelBody u = StdLayout EscapedText
                 | KernTextH [KerningChar u]
                 | KernTextV [KerningChar u]
  deriving (Eq,Show)

type DLabelBody = LabelBody Double


-- | A Char (possibly escaped) paired with is displacement from 
-- the previous KerningChar.
--
type KerningChar u = (u,EscapedChar) 

type DKerningChar = KerningChar Double

-- | Ellipse represented by center and half_width * half_height.
--
-- Center is the dx * dy of the PrimCTM.
--
data PrimEllipse u = PrimEllipse 
      { ellipse_half_width    :: u
      , ellipse_half_height   :: u 
      , ellipse_ctm           :: PrimCTM u
      } 
  deriving (Eq,Show)



--------------------------------------------------------------------------------
-- Graphics state datatypes

-- | Graphics state used by the rendering monads.
--
-- This type is hidden by the top-level module @Wumpus.Core@.
--
data GraphicsState = GraphicsState
      { gs_draw_colour  :: RGBi
      , gs_font_size    :: Int
      , gs_font_face    :: FontFace
      , gs_stroke_attr  :: StrokeAttr 
      }
  deriving (Eq,Show)


--------------------------------------------------------------------------------
-- family instances

type instance DUnit (Picture u)     = u
type instance DUnit (Primitive u)   = u
type instance DUnit (PrimEllipse u) = u
type instance DUnit (PrimLabel u)   = u
type instance DUnit (PrimPath u)    = u

--------------------------------------------------------------------------------
-- instances


instance (Num u, PSUnit u) => Format (Picture u) where
  format (Leaf m prims)     = indent 2 $ vcat [ text "** Leaf-pic **"
                                              , fmtLocale m 
                                              , fmtPrimlist prims ]

  format (Picture m pics)   = indent 2 $ vcat [ text "** Tree-pic **"
                                              , fmtLocale m
                                              , fmtPics pics ]
 
  format (Clip m path pic)  = indent 2 $ vcat [ text "** Clip-path **"
                                              , fmtLocale m
                                              , format path
                                              , format pic  ]


fmtPics :: PSUnit u => JoinList (Picture u) -> Doc
fmtPics ones = snd $ F.foldl' fn (0,empty) ones
  where
    fn (n,acc) e = (n+1, vcat [ acc, text "-- " <+> int n, format e, line])


fmtLocale :: (Num u, PSUnit u) => Locale u -> Doc
fmtLocale (bb,_) = format bb


instance PSUnit u => Format (Primitive u) where
  format (PPath props p)    = 
      indent 2 $ vcat [ text "path:" <+> format props, format p ]

  format (PLabel props l)   =
      indent 2 $ vcat [ text "label:" <+> format props, format l ]

  format (PEllipse props e) = 
      indent 2 $ vcat [ text "ellipse:" <+> format props, format e ]

  format (PContext _ a)     = 
      vcat [ text "-- svg ctx change " , format a ]

  format (PSVG _ a)       = 
      vcat [ text "-- svg:", format  a ]

  format (PGroup ones)      = 
      vcat [ text "-- group ", fmtPrimlist ones  ]


fmtPrimlist :: PSUnit u => JoinList (Primitive u) -> Doc
fmtPrimlist ones = snd $ F.foldl' fn (0,empty) ones
  where
    fn (n,acc) e = (n+1, vcat [ acc, text "-- leaf" <+> int n, format e, line])


instance PSUnit u => Format (PrimPath u) where
   format (PrimPath pt ps) = vcat (start : map format ps)
      where
        start = text "start_point " <> format pt

instance PSUnit u => Format (PrimPathSegment u) where
  format (RelCurveTo p1 p2 p3)  =
    text "rel_curve_to " <> format p1 <+> format p2 <+> format p3

  format (RelLineTo pt)         = text "rel_line_to  " <> format pt

instance PSUnit u => Format (PrimLabel u) where
  format (PrimLabel s ctm) = 
     vcat [ dquotes (format s)
          , text "ctm="           <> format ctm
          ]

instance PSUnit u => Format (LabelBody u) where
  format (StdLayout enctext) = format enctext
  format (KernTextH xs)      = text "(KernH)" <+> hcat (map (format .snd) xs)
  format (KernTextV xs)      = text "(KernV)" <+> hcat (map (format .snd) xs)


instance PSUnit u => Format (PrimEllipse u) where
  format (PrimEllipse hw hh ctm) =  text "hw="       <> dtruncFmt hw
                                <+> text "hh="       <> dtruncFmt hh
                                <+> text "ctm="      <> format ctm
  

instance Format XLink where
  format (XLink ss) = text "xlink:href" <+> text ss


--------------------------------------------------------------------------------

instance Boundary (Picture u) where
  boundary (Leaf    (bb,_) _)   = bb
  boundary (Picture (bb,_) _)   = bb
  boundary (Clip    (bb,_) _ _) = bb


instance (Real u, Floating u, FromPtSize u) => Boundary (Primitive u) where
  boundary (PPath _ p)      = pathBoundary p
  boundary (PLabel a l)     = labelBoundary (label_font a) l
  boundary (PEllipse _ e)   = ellipseBoundary e
  boundary (PContext _ a)   = boundary a
  boundary (PSVG _ a)       = boundary a
  boundary (PGroup ones)    = outer $ viewl ones 
    where
      outer (OneL a)     = boundary a
      outer (a :< as)    = inner (boundary a) (viewl as)

      inner bb (OneL a)  = bb `boundaryUnion` boundary a
      inner bb (a :< as) = inner (bb `boundaryUnion` boundary a) (viewl as)




pathBoundary :: (Num u, Ord u) => PrimPath u -> BoundingBox u
pathBoundary (PrimPath st xs) = step st (st,st) xs
  where
    step _  (lo,hi) []                         = BBox lo hi 

    step pt (lo,hi) (RelLineTo v1:rest)        = 
        let p1 = pt .+^ v1
        in step p1 (lo2 lo p1, hi2 hi p1) rest

    step pt (lo,hi) (RelCurveTo v1 v2 v3:rest) = 
        let p1  = pt .+^ v1
            p2  = p1 .+^ v2
            p3  = p2 .+^ v3
            lo' = lo4 lo p1 p2 p3 
            hi' = hi4 hi p1 p2 p3
        in step p3 (lo',hi') rest 

    lo2 (P2 x1 y1) (P2 x2 y2) = P2 (min x1 x2) (min y1 y2)

    hi2 (P2 x1 y1) (P2 x2 y2) = P2 (max x1 x2) (max y1 y2)

    lo4 (P2 x1 y1) (P2 x2 y2) (P2 x3 y3) (P2 x4 y4) = 
        P2 (min x1 $ min x2 $ min x3 x4) (min y1 $ min y2 $ min y3 y4) 

    hi4 (P2 x1 y1) (P2 x2 y2) (P2 x3 y3) (P2 x4 y4) = 
        P2 (max x1 $ max x2 $ max x3 x4) (max y1 $ max y2 $ max y3 y4) 
 


labelBoundary :: (Floating u, Real u, FromPtSize u) 
              => FontAttr -> PrimLabel u -> BoundingBox u
labelBoundary attr (PrimLabel body ctm) = 
    retraceBoundary (m33 *#) untraf_bbox
  where
    m33         = matrixRepCTM ctm
    untraf_bbox = labelBodyBoundary (font_size attr) body

labelBodyBoundary :: (Num u, Ord u, FromPtSize u) 
                  => FontSize -> LabelBody u -> BoundingBox u
labelBodyBoundary sz (StdLayout etxt) = stdLayoutBB sz etxt
labelBodyBoundary sz (KernTextH xs)   = hKerningBB sz xs
labelBodyBoundary sz (KernTextV xs)   = vKerningBB sz xs


stdLayoutBB :: (Num u, Ord u, FromPtSize u) 
            => FontSize -> EscapedText -> BoundingBox u
stdLayoutBB sz etxt = textBoundsEsc sz zeroPt etxt


-- Note - this assumes positive deltas (and a nonempty list)...
--
-- Kern deltas are relative to the left basepoint, so they are
-- irrespective of the actual charater width. Thus to calculate
-- the bounding box Wumpus calculates the bounds of one character
-- then expands the right edge with the sum of the (rightwards)
-- displacements.
-- 
hKerningBB :: (Num u, Ord u, FromPtSize u) 
           => FontSize -> [(u,EscapedChar)] -> BoundingBox u
hKerningBB sz xs = rightGrow (sumDiffs xs) $ textBounds sz zeroPt "A"
  where
    sumDiffs                          = foldr (\(u,_) i -> i+u)  0
    rightGrow u (BBox ll (P2 x1 y1))  = BBox ll (P2 (x1+u) y1)


-- Note - likewise same assumptions as horizontal version.
-- (A postive distance represents a move downwards)...
--
-- The kern delta is the distance between baselines of successive
-- characters, so character height is irrespective when summing 
-- the deltas.
-- 
-- Also note, that the Label /grows/ downwards...
--
vKerningBB :: (Num u, Ord u, FromPtSize u) 
           => FontSize -> [(u,EscapedChar)] -> BoundingBox u
vKerningBB sz xs = downGrow (sumDiffs xs) $ textBounds sz zeroPt "A"
  where
    sumDiffs                                = foldr (\(u,_) i -> i+u)  0
    downGrow u (BBox (P2 x0 y0) (P2 x1 y1)) = BBox (P2 x0 (y0-u)) (P2 x1 y1)


-- | Ellipse bbox is the bounding rectangle, rotated as necessary 
-- then retraced.
--
ellipseBoundary :: (Real u, Floating u) => PrimEllipse u -> BoundingBox u
ellipseBoundary (PrimEllipse hw hh ctm) = 
    traceBoundary $ map (m33 *#) [sw,se,ne,nw]
  where
    sw   = P2 (-hw) (-hh) 
    se   = P2   hw  (-hh) 
    ne   = P2   hw    hh 
    nw   = P2 (-hw)   hh 
    m33  = matrixRepCTM ctm


--------------------------------------------------------------------------------
-- Affine transformations

-- Affine transformation of Pictures only transforms the 
-- BoundingBox, the primitives within the picture are untouched.
-- The transformation is transmitted to PostScript as a matrix 
-- update (frame change).
--

instance (Num u, Ord u) => Transform (Picture u) where
  transform mtrx = 
    mapLocale $ \(bb,xs) -> (transform mtrx bb, Matrix mtrx:xs)

instance (Real u, Floating u) => Rotate (Picture u) where
  rotate theta = 
    mapLocale $ \(bb,xs) -> (rotate theta bb, Rotate theta:xs)


instance (Real u, Floating u) => RotateAbout (Picture u) where
  rotateAbout theta pt = 
    mapLocale $ \(bb,xs) -> (rotateAbout theta pt bb, RotAbout theta pt:xs)

instance (Num u, Ord u) => Scale (Picture u) where
  scale sx sy = 
    mapLocale $ \(bb,xs) -> (scale sx sy bb, Scale sx sy : xs)

instance (Num u, Ord u) => Translate (Picture u) where
  translate dx dy = 
    mapLocale $ \(bb,xs) -> (translate dx dy bb, Translate dx dy:xs)


mapLocale :: (Locale u -> Locale u) -> Picture u -> Picture u
mapLocale f (Leaf lc ones)     = Leaf (f lc) ones
mapLocale f (Picture lc ones)  = Picture (f lc) ones
mapLocale f (Clip lc pp pic)   = Clip (f lc) pp pic


--------------------------------------------------------------------------------
-- Transform primitives


-- Note - Primitives are not instances of transform
--
-- (ShapeCTM is not a real matrix).
-- 

instance (Real u, Floating u) => Rotate (Primitive u) where
  rotate r (PPath a path)   = PPath a    $ rotatePath r path
  rotate r (PLabel a lbl)   = PLabel a   $ rotateLabel r lbl
  rotate r (PEllipse a ell) = PEllipse a $ rotateEllipse r ell
  rotate r (PContext a chi) = PContext a $ rotate r chi 
  rotate r (PSVG a chi)     = PSVG a     $ rotate r chi 
  rotate r (PGroup xs)      = PGroup     $ fmap (rotate r) xs
 

instance (Real u, Floating u) => RotateAbout (Primitive u) where
  rotateAbout r pt (PPath a path)   = PPath a    $ rotateAboutPath r pt path
  rotateAbout r pt (PLabel a lbl)   = PLabel a   $ rotateAboutLabel r pt lbl
  rotateAbout r pt (PEllipse a ell) = PEllipse a $ rotateAboutEllipse r pt ell
  rotateAbout r pt (PContext a chi) = PContext a $ rotateAbout r pt chi
  rotateAbout r pt (PSVG a chi)     = PSVG a     $ rotateAbout r pt chi
  rotateAbout r pt (PGroup xs)      = PGroup     $ fmap (rotateAbout r pt) xs


instance Num u => Scale (Primitive u) where
  scale sx sy (PPath a path)    = PPath a    $ scalePath sx sy path
  scale sx sy (PLabel a lbl)    = PLabel a   $ scaleLabel sx sy lbl
  scale sx sy (PEllipse a ell)  = PEllipse a $ scaleEllipse sx sy ell
  scale sx sy (PContext a chi)  = PContext a $ scale sx sy chi
  scale sx sy (PSVG a chi)      = PSVG a     $ scale sx sy chi
  scale sx sy (PGroup xs)       = PGroup     $ fmap (scale sx sy) xs


instance Num u => Translate (Primitive u) where
  translate dx dy (PPath a path)   = PPath a    $ translatePath dx dy path
  translate dx dy (PLabel a lbl)   = PLabel a   $ translateLabel dx dy lbl
  translate dx dy (PEllipse a ell) = PEllipse a $ translateEllipse dx dy ell
  translate dx dy (PContext a chi) = PContext a $ translate dx dy chi
  translate dx dy (PSVG a chi)     = PSVG a     $ translate dx dy chi
  translate dx dy (PGroup xs)      = PGroup     $ fmap (translate dx dy) xs


--------------------------------------------------------------------------------
-- Paths


rotatePath :: (Real u, Floating u) => Radian -> PrimPath u -> PrimPath u
rotatePath ang = mapPath (rotate ang) (rotate ang)


rotateAboutPath :: (Real u, Floating u) 
                => Radian -> Point2 u -> PrimPath u -> PrimPath u
rotateAboutPath ang pt = mapPath (rotateAbout ang pt) (rotateAbout ang pt) 


scalePath :: Num u => u -> u -> PrimPath u -> PrimPath u
scalePath sx sy = mapPath (scale sx sy) (scale sx sy)

-- Note - translate only needs change the start point /because/ 
-- the path represented as a relative path.
-- 
translatePath :: Num u => u -> u -> PrimPath u -> PrimPath u
translatePath x y (PrimPath st xs) = PrimPath (translate x y st) xs


mapPath :: (Point2 u -> Point2 u) -> (Vec2 u -> Vec2 u) 
        -> PrimPath u -> PrimPath u
mapPath f g (PrimPath st xs) = PrimPath (f st) (map (mapSeg g) xs)

mapSeg :: (Vec2 u -> Vec2 u) -> PrimPathSegment u -> PrimPathSegment u
mapSeg fn (RelLineTo p)         = RelLineTo (fn p)
mapSeg fn (RelCurveTo p1 p2 p3) = RelCurveTo (fn p1) (fn p2) (fn p3)

--------------------------------------------------------------------------------
-- Labels



-- Rotate the baseline-left start point _AND_ the CTM of the 
-- label.
--
rotateLabel :: (Real u, Floating u) 
            => Radian -> PrimLabel u -> PrimLabel u
rotateLabel ang (PrimLabel txt ctm) = PrimLabel txt (rotateCTM ang ctm)


-- /rotateAbout/ the start-point, /rotate/ the the CTM.
--
rotateAboutLabel :: (Real u, Floating u) 
                 => Radian -> Point2 u -> PrimLabel u -> PrimLabel u
rotateAboutLabel ang pt (PrimLabel txt ctm) = 
    PrimLabel txt (rotateAboutCTM ang pt ctm)


scaleLabel :: Num u => u -> u -> PrimLabel u -> PrimLabel u
scaleLabel sx sy (PrimLabel txt ctm) = PrimLabel txt (scaleCTM sx sy ctm)


-- Change the bottom-left corner.
--
translateLabel :: Num u => u -> u -> PrimLabel u -> PrimLabel u
translateLabel dx dy (PrimLabel txt ctm) = 
    PrimLabel txt (translateCTM dx dy ctm)

--------------------------------------------------------------------------------
-- Ellipse


rotateEllipse :: (Real u, Floating u) 
              => Radian -> PrimEllipse u -> PrimEllipse u
rotateEllipse ang (PrimEllipse hw hh ctm) = 
    PrimEllipse hw hh (rotateCTM ang ctm)
    

rotateAboutEllipse :: (Real u, Floating u) 
              => Radian -> Point2 u -> PrimEllipse u -> PrimEllipse u
rotateAboutEllipse ang pt (PrimEllipse hw hh ctm) = 
    PrimEllipse hw hh (rotateAboutCTM ang pt ctm)


scaleEllipse :: Num u => u -> u -> PrimEllipse u -> PrimEllipse u
scaleEllipse sx sy (PrimEllipse hw hh ctm) = 
    PrimEllipse hw hh (scaleCTM sx sy ctm)
    


-- Change the point
--
translateEllipse :: Num u => u -> u -> PrimEllipse u -> PrimEllipse u
translateEllipse dx dy (PrimEllipse hw hh ctm) = 
    PrimEllipse hw hh (translateCTM dx dy ctm)



--------------------------------------------------------------------------------
-- Additional operations


-- | Destructor for Matrix3'3.
-- 
-- Pattern matching on 6-tuple may be more convenient than using 
-- the Matrix3'3 directly.
--
-- > (M3'3 e0x e1x ox  
-- >       e0y e1y oy  
-- >       _   _   _  ) = (e0x,e0y,  e1x,e1y,  ox,oy)
--  
deconsMatrix :: Matrix3'3 u -> (u,u,u,u,u,u)
deconsMatrix (M3'3 e0x e1x ox  
                   e0y e1y oy  
                   _   _   _  ) = (e0x,e0y,  e1x,e1y,  ox,oy)



-- If a picture has coordinates smaller than (P2 4 4) then it 
-- needs repositioning before it is drawn to PostScript or SVG.
-- 
-- (P2 4 4) gives a 4 pt margin - maybe it sould be (0,0) or 
-- user defined.
--
repositionDeltas :: (Num u, Ord u) 
                 => Picture u -> (BoundingBox u, Maybe (Vec2 u))
repositionDeltas = step . boundary 
  where
    step bb@(BBox (P2 llx lly) (P2 urx ury))
        | llx < 4 || lly < 4  = (BBox ll ur, Just $ V2 x y)
        | otherwise           = (bb, Nothing)
      where 
        x  = 4 - llx
        y  = 4 - lly
        ll = P2 (llx+x) (lly+y)
        ur = P2 (urx+x) (ury+y) 


--------------------------------------------------------------------------------

-- | The initial graphics state.
--
-- PostScript has no default font so we always want the first 
-- /delta/ operation not to find a match and cause a @findfint@
-- command to be generated (PostScript @findfont@ commands are 
-- only written in the output on /deltas/ to reduce the 
-- output size).
--
zeroGS ::  GraphicsState 
zeroGS = GraphicsState { gs_draw_colour  = black
                       , gs_font_size    = (-1)
                       , gs_font_face    = unmatchable_face
                       , gs_stroke_attr  = default_stroke_attr
                       }
  where
    unmatchable_face = FontFace "DONT_MATCH"     "" 
                                SVG_BOLD_OBLIQUE no_encoding

    no_encoding      = IntMap.empty 


-- | Is the path empty - if so we might want to avoid printing it.
--
isEmptyPath :: PrimPath u -> Bool
isEmptyPath (PrimPath _ xs) = null xs

-- | Is the label empty - if so we might want to avoid printing it.
--
isEmptyLabel :: PrimLabel u -> Bool
isEmptyLabel (PrimLabel txt _) = body txt
   where
     body (StdLayout esc) = destrEscapedText null esc
     body (KernTextH xs)  = null xs
     body (KernTextV xs)  = null xs

