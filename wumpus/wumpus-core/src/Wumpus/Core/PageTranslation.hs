{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.PageTranslation
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Core page translation for SVG.
--
-- Note - initially an optimized translate was planned - smaller 
-- SVG code size and less use of /rectifying/ transformations for 
-- text. However working out a translation scheme proved to be 
-- tricky and Wumpus is stuck with the /trivialTransformation/.
--
--------------------------------------------------------------------------------


module Wumpus.Core.PageTranslation
  ( 

    trivialTranslation

  ) where

import Wumpus.Core.AffineTrans
import Wumpus.Core.PictureInternal
import Wumpus.Core.TrafoInternal


--------------------------------------------------------------------------------
-- trivial translation

-- Rescale the entire image (1,-1), rescale the ellipse and 
-- label primitives as they are encoutered (1,-1). No need to 
-- worry about scaling the BoundingBox
--

trivialTranslation :: (Num u, Ord u) => Picture u -> Picture u
trivialTranslation pic = 
   scale 1 (negate 1) (trivPic pic)

trivPic :: Num u => Picture u -> Picture u
trivPic (Leaf lc ones)      = Leaf lc $ fmap trivPrim ones
trivPic (Picture lc ones)   = Picture lc $ fmap trivPic ones

trivPrim :: Num u => Primitive u -> Primitive u
trivPrim (PPath a pp)     = PPath a pp
trivPrim (PLabel a lbl)   = PLabel a (trivLabel lbl)
trivPrim (PEllipse a ell) = PEllipse a (trivEllipse ell)
trivPrim (PContext a chi) = PContext a (trivPrim chi)
trivPrim (PSVG a chi)     = PSVG a (trivPrim chi)
trivPrim (PGroup ones)    = PGroup $ fmap trivPrim ones
trivPrim (PClip pp chi)   = PClip pp (trivPrim chi)


trivLabel :: Num u => PrimLabel u -> PrimLabel u
trivLabel (PrimLabel txt ctm) = PrimLabel txt (trivPrimCTM ctm)

trivEllipse :: Num u => PrimEllipse u -> PrimEllipse u
trivEllipse (PrimEllipse hw hh ctm) = PrimEllipse hw hh (trivPrimCTM ctm)

-- Is the translation here just negating the angle with scaling
-- left untouched?
--
trivPrimCTM :: Num u => PrimCTM u -> PrimCTM u
trivPrimCTM (PrimCTM dx dy sx sy theta) = PrimCTM dx dy sx (-sy) theta

