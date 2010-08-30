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

    translatePageCoordinates

  ) where


import Wumpus.Fresh.BoundingBox
import Wumpus.Fresh.Geometry
import Wumpus.Fresh.PictureInternal

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

