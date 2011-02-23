{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.PictureInternal
-- Copyright   :  (c) Stephen Tetley 2009-2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
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
  , DLocale
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
  , DPrimEllipse

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
import Wumpus.Core.Text.Base
import Wumpus.Core.TrafoInternal
import Wumpus.Core.Units
import Wumpus.Core.Utils.FormatCombinators
import Wumpus.Core.Utils.JoinList


import Data.AffineSpace                         -- package: vector-space

import qualified Data.Foldable                  as F
import qualified Data.IntMap                    as IntMap


-- | Picture is a rose tree. Leaves themselves are attributed
-- with colour, line-width etc. Picture is parametric on the unit 
-- type of points. The unit is typically @Double@ which 
-- corresponds to PostScripts /Point/ unit. For dogmatic reasons
-- Wumpus also has a newtype wrapper of Double to make a specific 
-- 'PtSize' type, but it is usually more convenient to just use 
-- @Double@.
-- 
-- By attributing leaves with their drawing properties, Wumpus\'s 
-- picture representaion is not directly matched to PostScript.
-- PostScript has a global graphics state (that allows local 
-- modifaction) from where drawing properties are inherited.
-- Wumpus has no attribute inheritance.
--
-- Omitting some details of the list representation, Picture is a 
-- simple non-empty rose tree via:
-- 
-- > tree = Leaf [primitive] | Picture [tree]
--
data Picture u = Leaf     (Locale u)              (JoinList (Primitive u))
               | Picture  (Locale u)              (JoinList (Picture u))
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
type Locale u = (BoundingBox u, [AffineTrafo])

type DLocale = Locale Double


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
-- Clipping is represented by a pair of the clipping path and
-- the primitive embedded within the path.
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
                 | PClip   (PrimPath u)       (Primitive u)
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
--
data PrimLabel u = PrimLabel 
      { label_body          :: LabelBody u
      , label_ctm           :: PrimCTM
      }
  deriving (Eq,Show)

--
-- Design note - the CTM unit type is fixed to Double (PS point) 
-- rather than parametric on unit.
--
-- This is so it is transmitted \"obviously\" to PostScript and 
-- SVG rather than being scaled according to the unit type.
--
-- E.g. converting a Pica to a PostScript point is (12*) but we 
-- don\'t want to convert the identity matrix as it will become
-- a scaling matrix: @[12,0,0,12,0,0]@.
-- 


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
      , ellipse_ctm           :: PrimCTM
      } 
  deriving (Eq,Show)

type DPrimEllipse = PrimEllipse Double

--
-- Design note - the CTM unit type is fixed to Double (PS point) 
-- rather than parametric on unit.
--
-- For the rationale see the PrimLabel design note.
-- 
 

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

-- Perhaps the only useful functor operation is unit change.



instance Functor Picture where
  fmap f (Leaf loca ones)    = Leaf (fmapLocale f loca) (fmap (fmap f) ones)
  fmap f (Picture loca ones) = Picture (fmapLocale f loca) (fmap (fmap f) ones)


fmapLocale :: (u -> u1) -> Locale u -> Locale u1
fmapLocale f (bb, dtrafos) = (fmap f bb, dtrafos)

instance Functor Primitive where
  fmap f (PPath a p)       = PPath a (fmap f p)
  fmap f (PLabel a l)      = PLabel a (fmap f l)
  fmap f (PEllipse a e)    = PEllipse a (fmap f e)
  fmap f (PContext fcxt p) = PContext fcxt (fmap f p)
  fmap f (PSVG a p)        = PSVG a (fmap f p)
  fmap f (PGroup ones)     = PGroup (fmap (fmap f) ones)
  fmap f (PClip pp p)      = PClip (fmap f pp) (fmap f p) 
  



instance Functor PrimPath where 
   fmap f (PrimPath p0 ps) = PrimPath (fmap f p0) (map (fmap f) ps)


instance Functor PrimPathSegment where
  fmap f (RelCurveTo v1 v2 v3) = RelCurveTo (fmap f v1) (fmap f v2) (fmap f v3)
  fmap f (RelLineTo v1)        = RelLineTo (fmap f v1)



instance Functor AbsPathSegment where
  fmap f (AbsCurveTo p1 p2 p3) = AbsCurveTo (fmap f p1) (fmap f p2) (fmap f p3)
  fmap f (AbsLineTo p1)        = AbsLineTo  (fmap f p1)



instance Functor PrimLabel where
  fmap f (PrimLabel body dctm) = PrimLabel (fmap f body) dctm


instance Functor LabelBody where
  fmap _ (StdLayout esc) = StdLayout esc
  fmap f (KernTextH xs)  = KernTextH $ map (\(u,a) -> (f u, a)) xs
  fmap f (KernTextV xs)  = KernTextV $ map (\(u,a) -> (f u, a)) xs



instance Functor PrimEllipse where
  fmap f (PrimEllipse hw hh dctm) = PrimEllipse (f hw) (f hh) dctm 

-- format

instance Format u => Format (Picture u) where
  format (Leaf m prims)     = indent 2 $ vcat [ text "** Leaf-pic **"
                                              , fmtLocale m 
                                              , fmtPrimlist prims ]

  format (Picture m pics)   = indent 2 $ vcat [ text "** Tree-pic **"
                                              , fmtLocale m
                                              , fmtPics pics ]
 

fmtPics :: Format u => JoinList (Picture u) -> Doc
fmtPics ones = snd $ F.foldl' fn (0,empty) ones
  where
    fn (n,acc) e = (n+1, vcat [ acc, text "-- " <+> int n, format e, line])


fmtLocale :: Format u => Locale u -> Doc
fmtLocale (bb,_) = format bb


instance Format u => Format (Primitive u) where
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

  format (PClip path pic)  = 
      vcat [ text "-- clip-path ", format path, format pic  ]



fmtPrimlist :: Format u => JoinList (Primitive u) -> Doc
fmtPrimlist ones = snd $ F.foldl' fn (0,empty) ones
  where
    fn (n,acc) e = (n+1, vcat [ acc, text "-- leaf" <+> int n, format e, line])


instance Format u => Format (PrimPath u) where
   format (PrimPath pt ps) = vcat (start : map format ps)
      where
        start = text "start_point " <> format pt

instance Format u => Format (PrimPathSegment u) where
  format (RelCurveTo p1 p2 p3)  =
    text "rel_curve_to " <> format p1 <+> format p2 <+> format p3

  format (RelLineTo pt)         = text "rel_line_to  " <> format pt

instance Format u => Format (PrimLabel u) where
  format (PrimLabel s ctm) = 
     vcat [ dquotes (format s)
          , text "ctm="           <> format ctm
          ]

instance Format u => Format (LabelBody u) where
  format (StdLayout enctext) = format enctext
  format (KernTextH xs)      = text "(KernH)" <+> hcat (map (format .snd) xs)
  format (KernTextV xs)      = text "(KernV)" <+> hcat (map (format .snd) xs)


instance Format u => Format (PrimEllipse u) where
  format (PrimEllipse hw hh ctm) =  text "hw="       <> format hw
                                <+> text "hh="       <> format hh
                                <+> text "ctm="      <> format ctm
  

instance Format XLink where
  format (XLink ss) = text "xlink:href" <+> text ss


--------------------------------------------------------------------------------

instance Boundary (Picture u) where
  boundary (Leaf    (bb,_) _)   = bb
  boundary (Picture (bb,_) _)   = bb


instance (Real u, Floating u, PtSize u) => Boundary (Primitive u) where
  boundary (PPath _ p)      = pathBoundary p
  boundary (PLabel a l)     = labelBoundary (label_font a) l
  boundary (PEllipse _ e)   = ellipseBoundary e
  boundary (PContext _ a)   = boundary a
  boundary (PSVG _ a)       = boundary a
  boundary (PClip p _)      = pathBoundary p
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
 


labelBoundary :: (Floating u, Real u, PtSize u) 
              => FontAttr -> PrimLabel u -> BoundingBox u
labelBoundary attr (PrimLabel body ctm) = 
    retraceBoundary (m33 *#) untraf_bbox
  where
    m33         = fmap dpoint $ matrixRepCTM ctm
    untraf_bbox = labelBodyBoundary (font_size attr) body

labelBodyBoundary :: (Num u, Ord u, PtSize u) 
                  => FontSize -> LabelBody u -> BoundingBox u
labelBodyBoundary sz (StdLayout etxt) = stdLayoutBB sz etxt
labelBodyBoundary sz (KernTextH xs)   = hKerningBB sz xs
labelBodyBoundary sz (KernTextV xs)   = vKerningBB sz xs


stdLayoutBB :: (Num u, Ord u, PtSize u) 
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
hKerningBB :: (Num u, Ord u, PtSize u) 
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
vKerningBB :: (Ord u, PtSize u) 
           => FontSize -> [(u,EscapedChar)] -> BoundingBox u
vKerningBB sz xs = downGrow (sumDiffs xs) $ textBounds sz zeroPt "A"
  where
    sumDiffs                                = foldr (\(u,_) i -> i+u)  0
    downGrow u (BBox (P2 x0 y0) (P2 x1 y1)) = BBox (P2 x0 (y0-u)) (P2 x1 y1)


-- | Ellipse bbox is the bounding rectangle, rotated as necessary 
-- then retraced.
--
ellipseBoundary :: (Real u, Floating u, PtSize u) 
                => PrimEllipse u -> BoundingBox u
ellipseBoundary (PrimEllipse hw hh ctm) = 
    traceBoundary $ map (m33 *#) [sw,se,ne,nw]
  where
    sw   = P2 (-hw) (-hh) 
    se   = P2   hw  (-hh) 
    ne   = P2   hw    hh 
    nw   = P2 (-hw)   hh 
    m33  = fmap dpoint $ matrixRepCTM ctm


--------------------------------------------------------------------------------
-- Affine transformations

-- Affine transformation of Pictures only transforms the 
-- BoundingBox, the primitives within the picture are untouched.
-- The transformation is transmitted to PostScript as a matrix 
-- update (frame change).
--

instance (PtSize u, Ord u) => Transform (Picture u) where
  transform mtrx = 
    mapLocale $ \(bb,xs) -> let cmd = Matrix $ fmap psDouble mtrx
                            in (transform mtrx bb, cmd : xs)

instance (PtSize u, Ord u) => Rotate (Picture u) where
  rotate theta = 
    mapLocale $ \(bb,xs) -> (rotate theta bb, Rotate theta:xs)


instance (Real u, Floating u, PtSize u) => RotateAbout (Picture u) where
  rotateAbout theta pt@(P2 x y) = 
    mapLocale $ \(bb,xs) -> let dpt = P2 (psDouble x) (psDouble y)
                                cmd = RotAbout theta dpt
                            in (rotateAbout theta pt bb, cmd : xs)

instance (PtSize u, Ord u) => Scale (Picture u) where
  scale sx sy = 
    mapLocale $ \(bb,xs) -> let cmd = Scale (psDouble sx) (psDouble sy)
                            in (scale sx sy bb, cmd : xs)

instance (PtSize u, Ord u) => Translate (Picture u) where
  translate dx dy = 
    mapLocale $ \(bb,xs) -> let cmd = Translate (psDouble dx) (psDouble dy)
                            in ( translate dx dy bb, cmd : xs)
                     


mapLocale :: (Locale u -> Locale u) -> Picture u -> Picture u
mapLocale f (Leaf lc ones)     = Leaf (f lc) ones
mapLocale f (Picture lc ones)  = Picture (f lc) ones


--------------------------------------------------------------------------------
-- Transform primitives


-- Note - Primitives are not instances of transform
--
-- (ShapeCTM is not a real matrix).
-- 

instance (Real u, Floating u, PtSize u) => Rotate (Primitive u) where
  rotate r (PPath a path)   = PPath a    $ rotatePath r path
  rotate r (PLabel a lbl)   = PLabel a   $ rotateLabel r lbl
  rotate r (PEllipse a ell) = PEllipse a $ rotateEllipse r ell
  rotate r (PContext a chi) = PContext a $ rotate r chi 
  rotate r (PSVG a chi)     = PSVG a     $ rotate r chi 
  rotate r (PGroup xs)      = PGroup     $ fmap (rotate r) xs
  rotate r (PClip p chi)    = PClip (rotatePath r p) (rotate r chi)

instance PtSize u => RotateAbout (Primitive u) where
  rotateAbout r pt (PPath a path)   = PPath a    $ rotateAboutPath r pt path
  rotateAbout r pt (PLabel a lbl)   = PLabel a   $ rotateAboutLabel r pt lbl
  rotateAbout r pt (PEllipse a ell) = PEllipse a $ rotateAboutEllipse r pt ell
  rotateAbout r pt (PContext a chi) = PContext a $ rotateAbout r pt chi
  rotateAbout r pt (PSVG a chi)     = PSVG a     $ rotateAbout r pt chi
  rotateAbout r pt (PGroup xs)      = PGroup     $ fmap (rotateAbout r pt) xs
  rotateAbout r pt (PClip p chi)    = 
      PClip (rotateAboutPath r pt p) (rotateAbout r pt chi)


instance PtSize u => Scale (Primitive u) where
  scale sx sy (PPath a path)    = PPath a    $ scalePath sx sy path
  scale sx sy (PLabel a lbl)    = PLabel a   $ scaleLabel sx sy lbl
  scale sx sy (PEllipse a ell)  = PEllipse a $ scaleEllipse sx sy ell
  scale sx sy (PContext a chi)  = PContext a $ scale sx sy chi
  scale sx sy (PSVG a chi)      = PSVG a     $ scale sx sy chi
  scale sx sy (PGroup xs)       = PGroup     $ fmap (scale sx sy) xs
  scale sx sy (PClip p chi)     = PClip (scalePath sx sy p) (scale sx sy chi)


instance PtSize u => Translate (Primitive u) where
  translate dx dy (PPath a path)   = PPath a    $ translatePath dx dy path
  translate dx dy (PLabel a lbl)   = PLabel a   $ translateLabel dx dy lbl
  translate dx dy (PEllipse a ell) = PEllipse a $ translateEllipse dx dy ell
  translate dx dy (PContext a chi) = PContext a $ translate dx dy chi
  translate dx dy (PSVG a chi)     = PSVG a     $ translate dx dy chi
  translate dx dy (PGroup xs)      = PGroup     $ fmap (translate dx dy) xs
  translate dx dy (PClip p chi)    = 
      PClip (translatePath dx dy p) (translate dx dy chi)


--------------------------------------------------------------------------------
-- Paths

-- Affine transformations on paths are applied to their control
-- points. 

rotatePath :: (Real u, Floating u, PtSize u) => Radian -> PrimPath u -> PrimPath u
rotatePath ang = mapPath (rotate ang) (rotate ang)


rotateAboutPath :: PtSize u
                => Radian -> Point2 u -> PrimPath u -> PrimPath u
rotateAboutPath ang pt = mapPath (rotateAbout ang pt) (rotateAbout ang pt) 


scalePath :: PtSize u => Double -> Double -> PrimPath u -> PrimPath u
scalePath sx sy = mapPath (scale sx sy) (scale sx sy)

-- Note - translate only needs change the start point /because/ 
-- the path represented as a relative path.
-- 
translatePath :: PtSize u => Double -> Double -> PrimPath u -> PrimPath u
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
rotateAboutLabel :: (PtSize u1, PtSize u) 
                 => Radian -> Point2 u1 -> PrimLabel u -> PrimLabel u
rotateAboutLabel ang (P2 x y) (PrimLabel txt ctm) = 
    PrimLabel txt (rotateAboutCTM ang (P2 (psDouble x) (psDouble y)) ctm)


scaleLabel :: PtSize u => Double -> Double -> PrimLabel u -> PrimLabel u
scaleLabel sx sy (PrimLabel txt ctm) = 
    PrimLabel txt (scaleCTM (psDouble sx) (psDouble sy) ctm)


-- Change the bottom-left corner.
--
translateLabel :: PtSize u => Double -> Double -> PrimLabel u -> PrimLabel u
translateLabel dx dy (PrimLabel txt ctm) = 
    PrimLabel txt (translateCTM (psDouble dx) (psDouble dy) ctm)

--------------------------------------------------------------------------------
-- Ellipse


rotateEllipse :: (Real u, Floating u) 
              => Radian -> PrimEllipse u -> PrimEllipse u
rotateEllipse ang (PrimEllipse hw hh ctm) = 
    PrimEllipse hw hh (rotateCTM ang ctm)
    

rotateAboutEllipse :: (PtSize u1, PtSize u)
              => Radian -> Point2 u1 -> PrimEllipse u -> PrimEllipse u
rotateAboutEllipse ang (P2 x y) (PrimEllipse hw hh ctm) = 
    PrimEllipse hw hh (rotateAboutCTM ang (P2 (psDouble x) (psDouble y)) ctm)


scaleEllipse :: PtSize u => Double -> Double -> PrimEllipse u -> PrimEllipse u
scaleEllipse sx sy (PrimEllipse hw hh ctm) = 
    PrimEllipse hw hh (scaleCTM (psDouble sx) (psDouble sy) ctm)
    


-- Change the point
--
translateEllipse :: PtSize u => Double -> Double -> PrimEllipse u -> PrimEllipse u
translateEllipse dx dy (PrimEllipse hw hh ctm) = 
    PrimEllipse hw hh (translateCTM (psDouble dx) (psDouble dy) ctm)



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



-- If a picture has coordinates smaller than (P2 4 4) especially
-- negative ones then it needs repositioning before it is drawn 
-- to PostScript or SVG.
-- 
-- (P2 4 4) gives a 4 pt margin - maybe it sould be (0,0) or 
-- user defined.
--
repositionDeltas :: (PtSize u, Ord u) 
                 => Picture u -> (BoundingBox u, Maybe (Vec2 u))
repositionDeltas = step . boundary 
  where
    step bb@(BBox (P2 llx lly) (P2 urx ury))
        | llx < unit4 || lly < unit4  = (BBox ll ur, Just $ V2 x y)
        | otherwise                   = (bb, Nothing)
      where 
        x  = unit4 - llx
        y  = unit4 - lly
        ll = P2 (llx+x) (lly+y)
        ur = P2 (urx+x) (ury+y) 

    unit4 = dpoint 4


--------------------------------------------------------------------------------

-- | The initial graphics state.
--
-- PostScript has no default font so we always want the first 
-- /delta/ operation not to find a match and cause a @findfont@
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

