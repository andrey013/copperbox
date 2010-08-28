{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Fresh.PictureInternal
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


module Wumpus.Fresh.PictureInternal
  ( 

    Picture(..)
  , DPicture
  , Locale
  , AffineTrafo(..)
  , GSUpdate(..)

  , Primitive(..)
  , DPrimitive
  , XLink(..)

  , PrimPath(..)
  , PathProps(..)
  , PrimPathSegment(..)
  , PrimLabel(..)
  , LabelProps(..)
  , PrimEllipse(..)
  , EllipseProps(..)
  , PrimCTM(..)


  -- * PrimCTM
  , identityCTM
  , scaleCTM
  , rotateCTM
  , matrixRepCTM
  , translMatrixRepCTM

  , concatTrafos
  , deconsMatrix
  , repositionDeltas

  ) where

import Wumpus.Fresh.AffineTrans
import Wumpus.Fresh.BoundingBox
import Wumpus.Fresh.Colour
import Wumpus.Fresh.FontSize
import Wumpus.Fresh.FormatCombinators
import Wumpus.Fresh.Geometry
import Wumpus.Fresh.GraphicsState
import Wumpus.Fresh.OneList
import Wumpus.Fresh.PtSize
import Wumpus.Fresh.TextInternal
import Wumpus.Fresh.Utils


import Data.AffineSpace                         -- package: vector-space

import qualified Data.Foldable                  as F


-- For local shared graphics state updates add a new constructor:
-- Group (GS -> GS) (Picture u)

data Picture u = Leaf     (Locale u)              (OneList (Primitive u))
               | Picture  (Locale u)              (OneList (Picture u))
               | Clip     (Locale u) (PrimPath u) (Picture u)
               | Group    (Locale u) GSUpdate     (Picture u)
  deriving (Show)

newtype GSUpdate = GSUpdate { getGSU :: GraphicsState -> GraphicsState }

instance Show GSUpdate where
  show _ = "*function*"


type Locale u = (BoundingBox u, [AffineTrafo u])

type DPicture = Picture Double


data AffineTrafo u = Matrix (Matrix3'3 u)
                   | Rotate Radian
                   | RotAbout Radian (Point2 u)
                   | Scale u u
                   | Translate u u
  deriving (Eq,Show)                 


data Primitive u = PPath    PathProps    XLink (PrimPath u)
                 | PLabel   LabelProps   XLink (PrimLabel u)
                 | PEllipse EllipseProps XLink (PrimEllipse u)
  deriving (Eq,Show)

type DPrimitive = Primitive Double

data XLink = NoLink
           | XLinkHRef String
  deriving (Eq,Show)

-- | PrimPath - start point and a list of path segments.
--
data PrimPath u = PrimPath (Point2 u) [PrimPathSegment u]
  deriving (Eq,Show)


-- | PrimPathSegment - either a cubic Bezier curve or a line.
--
data PrimPathSegment u = PCurveTo  (Point2 u) (Point2 u) (Point2 u)
                       | PLineTo   (Point2 u)
  deriving (Eq,Show)


-- | Note when drawn /filled/ and drawn /stroked/ the same
-- polygon will have (slightly) different size:
--
-- * A filled shape fills /within/ the boundary of the shape
--
-- * A stroked shape draws a pen line around the boundary
--   of the shape. The actual size depends on the thickness
--   of the line (stroke width).
--
data PathProps = CFill RGB255 
               | CStroke [StrokeAttr] RGB255
               | OStroke [StrokeAttr] RGB255
               | CFillStroke RGB255 [StrokeAttr] RGB255
  deriving (Eq,Show)


-- | Label - represented by /baseline/ left point and text.
--
data PrimLabel u = PrimLabel 
      { label_baseline_left :: Point2 u
      , label_text          :: EncodedText
      , label_ctm           :: PrimCTM u
      }
  deriving (Eq,Show)


data LabelProps   = LabelProps 
      { label_colour :: RGB255
      , label_font   :: FontAttr
      }
  deriving (Eq,Ord,Show)


-- Ellipse represented by center and half_width * half_height
--
data PrimEllipse u = PrimEllipse 
      { ellipse_center        :: Point2 u
      , ellipse_half_width    :: u
      , ellipse_half_height   :: u 
      , ellipse_ctm           :: PrimCTM u
      } 
  deriving (Eq,Show)


-- | Ellipses and circles are always closed.
--
data EllipseProps = EFill RGB255
                  | EStroke [StrokeAttr] RGB255 

                  -- Note - first colour fill, second colour stroke.
                  | EFillStroke RGB255 [StrokeAttr] RGB255 
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


instance (Num u, PSUnit u) => Format (Picture u) where
  format (Leaf m prims)     = hangLines 2 [ text "** Leaf-pic **"
                                          , fmtLocale m 
                                          , fmtPrims prims ]

  format (Picture m pics)   = hangLines 2 [ text "** Tree-pic **"
                                          , fmtLocale m
                                          , fmtPics pics ]
 
  format (Clip m path pic)  = hangLines 2 [ text "** Clip-path **"
                                          , fmtLocale m
                                          , format path
                                          , format pic  ]

  format (Group m _ pic)    = hangLines 2 [ text "** Group **"
                                          , fmtLocale m
                                          , format pic  ]


fmtPics :: PSUnit u => OneList (Picture u) -> Doc
fmtPics ones = snd $ F.foldl' fn (0,empty) ones
  where
    fn (n,acc) e = (n+1, vcat [ acc, text "-- " <+> int n, format e, line])

fmtPrims :: PSUnit u => OneList (Primitive u) -> Doc
fmtPrims ones = snd $ F.foldl' fn (0,empty) ones
  where
    fn (n,acc) e = (n+1, vcat [ acc, text "-- leaf" <+> int n, format e, line])

fmtLocale :: (Num u, PSUnit u) => Locale u -> Doc
fmtLocale (bb,_) = format bb


instance PSUnit u => Format (Primitive u) where
  format (PPath props _ p)    = 
      hangLines 2 [ text "path:" <+> format props, format p ]

  format (PLabel props _ l)   =
      hangLines 2 [ text "label:" <+> format props, format l ]

  format (PEllipse props _ e) = 
      hangLines 2 [ text "ellipse:" <+> format props, format e ]


instance PSUnit u => Format (PrimPath u) where
   format (PrimPath pt ps) = vcat (start : map format ps)
      where
        start = text "start_point " <> format pt

instance PSUnit u => Format (PrimPathSegment u) where
  format (PCurveTo p1 p2 p3)  =
    text "curve_to    " <> format p1 <+> format p2 <+> format p3

  format (PLineTo pt)         = text "line_to     " <> format pt

instance PSUnit u => Format (PrimLabel u) where
  format (PrimLabel pt s ctm) = 
     vcat [ dquotes (format s)
          ,     text "baseline_left=" <> format pt
            <+> text "ctm="           <> format ctm
          ]

instance PSUnit u => Format (PrimEllipse u) where
  format (PrimEllipse ctr hw hh ctm) = text "center="   <> format ctr
                                   <+> text "hw="       <> dtruncFmt hw
                                   <+> text "hh="       <> dtruncFmt hh
                                   <+> text "ctm="      <> format ctm
  

instance PSUnit u => Format (PrimCTM u) where
  format (PrimCTM x y ang) = 
      parens (text "CTM" <+> text "sx="   <> dtruncFmt x 
                         <+> text "sy="   <> dtruncFmt y 
                         <+> text "ang="  <> format ang  )


instance Format PathProps where
  format (CFill rgb)          = format rgb <+> text "Fill"
  format (CStroke _ rgb)      = format rgb <+> text "Closed-stroke"
  format (OStroke _ rgb)      = format rgb <+> text "Open-stroke"
  format (CFillStroke f _ s)  = format f <+> text "Fill" <> char '/'
                            <+> format s <+> text "Stroke"   



instance Format LabelProps where
  format (LabelProps rgb attr) = format rgb 
                             <+> text (font_name $ font_face attr)

instance Format EllipseProps where
  format (EFill rgb)          = format rgb <+> text "Fill"
  format (EStroke _ rgb)      = format rgb <+> text "Stroke"
  format (EFillStroke f _ s)  = format f <+> text "Fill" <> char '/'
                            <+> format s <+> text "Stroke"   


--------------------------------------------------------------------------------

instance Boundary (Picture u) where
  boundary (Leaf (bb,_) _)    = bb
  boundary (Picture (bb,_) _) = bb
  boundary (Clip (bb,_) _ _)  = bb
  boundary (Group (bb,_) _ _) = bb

instance (Real u, Floating u, FromPtSize u) => Boundary (Primitive u) where
  boundary (PPath _ _ p)      = pathBoundary p
  boundary (PLabel a _ l)     = labelBoundary (label_font a) l
  boundary (PEllipse _ _ e)   = ellipseBoundary e



pathBoundary :: Ord u => PrimPath u -> BoundingBox u
pathBoundary (PrimPath st xs) = step (st,st) xs
  where
    step (lo,hi) []                       = BBox lo hi 
    step (lo,hi) (PLineTo p1:rest)        = step (lo2 lo p1, hi2 hi p1) rest
    step (lo,hi) (PCurveTo p1 p2 p3:rest) = let lo' = lo4 lo p1 p2 p3 
                                                hi' = hi4 hi p1 p2 p3
                                            in step (lo',hi') rest 

    lo2 (P2 x1 y1) (P2 x2 y2) = P2 (min x1 x2) (min y1 y2)

    hi2 (P2 x1 y1) (P2 x2 y2) = P2 (max x1 x2) (max y1 y2)

    lo4 (P2 x1 y1) (P2 x2 y2) (P2 x3 y3) (P2 x4 y4) = 
        P2 (min x1 $ min x2 $ min x3 x4) (min y1 $ min y2 $ min y3 y4) 

    hi4 (P2 x1 y1) (P2 x2 y2) (P2 x3 y3) (P2 x4 y4) = 
        P2 (max x1 $ max x2 $ max x3 x4) (max y1 $ max y2 $ max y3 y4) 
 


labelBoundary :: (Floating u, Real u, FromPtSize u) 
              => FontAttr -> PrimLabel u -> BoundingBox u
labelBoundary attr (PrimLabel (P2 x y) xs ctm) = 
    retraceBoundary  (disp . (m33 *#)) untraf_bbox
  where
    disp        = (.+^ V2 x y)
    m33         = matrixRepCTM ctm
    untraf_bbox = textBounds (font_size attr) zeroPt char_count
    char_count  = textLength xs



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
-- Affine transformations

instance (Num u, Ord u) => Transform (Picture u) where
  transform mtrx = 
    mapLocale (\(bb,xs) -> (transform mtrx bb, Matrix mtrx : xs))

instance (Real u, Floating u) => Rotate (Picture u) where
  rotate theta = 
    mapLocale (\(bb,xs) -> (rotate theta bb, Rotate theta : xs))

instance (Real u, Floating u) => RotateAbout (Picture u) where
  rotateAbout theta pt = 
    mapLocale (\(bb,xs) -> (rotateAbout theta pt bb, RotAbout theta pt : xs))

instance (Num u, Ord u) => Scale (Picture u) where
  scale sx sy = 
    mapLocale (\(bb,xs) -> (scale sx sy bb, Scale sx sy : xs))

instance (Num u, Ord u) => Translate (Picture u) where
  translate dx dy = 
    mapLocale (\(bb,xs) -> (translate dx dy bb, Translate dx dy : xs))


mapLocale :: (Locale u -> Locale u) -> Picture u -> Picture u
mapLocale f (Leaf lc ones)     = Leaf (f lc) ones
mapLocale f (Picture lc ones)  = Picture (f lc) ones
mapLocale f (Clip lc pp pic)   = Clip (f lc) pp pic
mapLocale f (Group lc upd pic) = Group (f lc) upd pic

--------------------------------------------------------------------------------
-- Manipulating the PrimCTM

identityCTM :: Num u => PrimCTM u
identityCTM = PrimCTM { ctm_scale_x = 1, ctm_scale_y = 1, ctm_rotation = 0 }



scaleCTM :: Num u => u -> u -> PrimCTM u -> PrimCTM u
scaleCTM x1 y1 (PrimCTM sx sy ang) = PrimCTM (x1*sx) (y1*sy) ang

rotateCTM :: Radian -> PrimCTM u -> PrimCTM u
rotateCTM ang1 (PrimCTM sx sy ang) = PrimCTM sx sy (circularModulo $ ang1+ang)

matrixRepCTM :: (Floating u, Real u) => PrimCTM u -> Matrix3'3 u
matrixRepCTM (PrimCTM sx sy ang) = 
    rotationMatrix (circularModulo ang) * scalingMatrix sx sy


-- Note - the order of combining a translation (i.e. the 
-- location of a point) and the CTM is crucial as matrix
-- multiplication is not commutative.
--
-- This function encapsulates the correct order.
--
translMatrixRepCTM :: (Floating u, Real u) 
                   => u -> u -> PrimCTM u -> Matrix3'3 u
translMatrixRepCTM x y ctm = translationMatrix x y * matrixRepCTM ctm


--------------------------------------------------------------------------------


concatTrafos :: (Floating u, Real u) => [AffineTrafo u] -> Matrix3'3 u
concatTrafos = foldr (\e ac -> matrixRepr e * ac) identityMatrix

matrixRepr :: (Floating u, Real u) => AffineTrafo u -> Matrix3'3 u
matrixRepr (Matrix mtrx)        = mtrx
matrixRepr (Rotate theta)       = rotationMatrix theta
matrixRepr (RotAbout theta pt)  = originatedRotationMatrix theta pt
matrixRepr (Scale sx sy)        = scalingMatrix sx sy 
matrixRepr (Translate dx dy)    = translationMatrix dx dy


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



-- This needs is for PostScript and SVG output - it should be 
-- hidden in the export list of Wumpus.Core


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





