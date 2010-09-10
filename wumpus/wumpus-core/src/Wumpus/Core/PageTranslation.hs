{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.PageTranslation
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Core page translation for SVG.
--
--------------------------------------------------------------------------------


module Wumpus.Core.PageTranslation
  ( 

    trivialTranslation

  ) where

import Wumpus.Core.AffineTrans
import Wumpus.Core.PictureInternal



--------------------------------------------------------------------------------
-- trivial translation

-- Rescale the entire image (1,-1), rescale the ellipse and 
-- label primitives as they are encoutered (1,-1). No need to 
-- worry about scaling the BoundingBox
--

trivialTranslation :: (Num u, Ord u) => Picture u -> Picture u
trivialTranslation pic = scale 1 (-1) (trivPic pic)

trivPic :: Num u => Picture u -> Picture u
trivPic (Leaf lc ones)      = Leaf lc $ fmap trivPrimElt ones
trivPic (Picture lc ones)   = Picture lc $ fmap trivPic ones
trivPic (Clip lc pp pic)    = Clip lc pp $ trivPic pic
trivPic (Group lc upd pic)  = Group lc upd $ trivPic pic

trivPrimElt :: Num u => PrimElement u -> PrimElement u
trivPrimElt (Atom prim)          = Atom (trivPrim prim)
trivPrimElt (XLinkGroup xl ones) = XLinkGroup xl $ fmap trivPrimElt ones

trivPrim :: Num u => Primitive u -> Primitive u
trivPrim (PPath a pp)     = PPath a pp
trivPrim (PLabel a lbl)   = PLabel a (trivLabel lbl)
trivPrim (PEllipse a ell) = PEllipse a (trivEllipse ell)


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

