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

  ) where

import Wumpus.Fresh.AffineTrans
import Wumpus.Fresh.PictureInternal



--------------------------------------------------------------------------------
-- trivial translation

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
