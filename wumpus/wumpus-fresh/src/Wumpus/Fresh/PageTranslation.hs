{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Fresh.PageTranslation
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Fresh page translation for SVG.
--
--------------------------------------------------------------------------------


module Wumpus.Fresh.PageTranslation
  ( 

    trivialTranslation
  , translatePageCoordinates

  ) where

import Wumpus.Fresh.AffineTrans
import Wumpus.Fresh.BoundingBox
import Wumpus.Fresh.Geometry
import Wumpus.Fresh.PictureInternal



--------------------------------------------------------------------------------
-- trivial translation

-- YUCK - the trivial translation seems a lot easier to perform 
-- /during/ drawing...
-- 
-- Rescale the entire image (1,-1), rescale the ellipse and 
-- label primitives as they are encoutered (1,-1). No need to 
-- worry about scaling the BoundingBox
--

trivialTranslation :: (Num u, Ord u) => Picture u -> Picture u
trivialTranslation pic = scale 1 (-1) (trivPic pic)

trivPic :: Num u => Picture u -> Picture u
trivPic (Leaf lc ones)      = Leaf lc $ fmap trivPrim ones
trivPic (Picture lc ones)   = Picture lc $ fmap trivPic ones
trivPic (Clip lc pp pic)    = Clip lc pp $ trivPic pic
trivPic (Group lc upd pic)  = Group lc upd $ trivPic pic

trivPrim :: Num u => Primitive u -> Primitive u
trivPrim (PPath a xl pp)     = PPath a xl pp
trivPrim (PLabel a xl lbl)   = PLabel a xl (trivLabel lbl)
trivPrim (PEllipse a xl ell) = PEllipse a xl (trivEllipse ell)


trivLabel :: Num u => PrimLabel u -> PrimLabel u
trivLabel (PrimLabel pt txt ctm) = PrimLabel pt txt (trivPrimCTM ctm)

trivEllipse :: Num u => PrimEllipse u -> PrimEllipse u
trivEllipse (PrimEllipse ctr hw hh ctm) = 
    PrimEllipse ctr hw hh (trivPrimCTM ctm)

-- Is the translation here just negating the angle with scaling
-- left untouched?
--
trivPrimCTM :: Num u => PrimCTM u -> PrimCTM u
trivPrimCTM (PrimCTM sx sy theta) = PrimCTM sx (-sy) theta


--------------------------------------------------------------------------------
-- THIS DOES NOT WORK!

translatePageCoordinates :: Num u => Picture u -> Picture u
translatePageCoordinates pic = transPic (y_max $ yrange pic) pic

transPic :: Num u => u -> Picture u -> Picture u
transPic ymax (Leaf lc ones)      = 
    Leaf (transLocale ymax lc) (fmap (transPrim ymax) ones)

transPic ymax (Picture lc ones)   = 
    Picture (transLocale ymax lc) (fmap (transPic ymax) ones)

transPic ymax (Clip lc pp pic)    =
    Clip (transLocale ymax lc) (transPath ymax pp) (transPic ymax pic)

transPic ymax (Group lc upd pic)  = 
    Group (transLocale ymax lc) upd (transPic ymax pic)


transLocale :: Num u => u -> Locale u -> Locale u
transLocale ymax (bb,xs,yr) = 
    (transBBox ymax bb, map transAT xs, yr)

transAT :: Num u => AffineTrafo u -> AffineTrafo u
transAT (Matrix  mtrx)      = Matrix $ scalingMatrix 1 (-1) * mtrx 
transAT (Rotate theta)      = Rotate $ negate theta
transAT (RotAbout theta pt) = RotAbout (negate theta) pt   -- TODO
transAT (Scale sx sy)       = Scale sx sy      -- THIS IS WRONG
transAT (Translate dx dy)   = Translate dx dy   -- TODO

transPrim :: Num u => u -> Primitive u -> Primitive u
transPrim ymax (PPath a xl pp)     = PPath a xl (transPath ymax pp)
transPrim ymax (PLabel a xl lbl)   = PLabel a xl (transLabel ymax lbl)
transPrim ymax (PEllipse a xl ell) = PEllipse a xl (transEllipse ymax ell)


transPath :: Num u => u -> PrimPath u -> PrimPath u
transPath ymax (PrimPath p0 segs) = 
    PrimPath (transPoint ymax p0) (map step segs)
  where
    fn                       = transPoint ymax
    step (PCurveTo p1 p2 p3) = PCurveTo (fn p1) (fn p2) (fn p3) 
    step (PLineTo p1)        = PLineTo (fn p1)

transLabel :: Num u => u -> PrimLabel u -> PrimLabel u
transLabel ymax (PrimLabel pt txt ctm) = 
    PrimLabel (transPoint ymax pt) txt (transPrimCTM ctm)

transEllipse :: Num u => u -> PrimEllipse u -> PrimEllipse u
transEllipse ymax (PrimEllipse ctr hw hh ctm) = 
    PrimEllipse (transPoint ymax ctr) hw hh (transPrimCTM ctm)

         

transBBox ::  Num u => u -> BoundingBox u -> BoundingBox u
transBBox ymax (BBox ll ur) = BBox (transPoint ymax ll) (transPoint ymax ur)

transPoint :: Num u => u -> Point2 u -> Point2 u
transPoint ymax (P2 x y) = P2 x (ymax - y) 

-- Is the translation here just negating the angle with scaling
-- left untouched?
--
transPrimCTM :: PrimCTM u -> PrimCTM u
transPrimCTM (PrimCTM sx sy theta) = PrimCTM sx sy (negate theta)

