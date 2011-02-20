{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Wumpus.Core.PageTranslation
-- Copyright   :  (c) Stephen Tetley 2010-2011
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
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
    psUnitTranslation
  , svgPageTranslation

  ) where

import Wumpus.Core.AffineTrans
import Wumpus.Core.PictureInternal
import Wumpus.Core.TrafoInternal
import Wumpus.Core.Units

--------------------------------------------------------------------------------
-- trivial translation

-- Rescale the entire image (1,-1), rescale the ellipse and 
-- label primitives as they are encoutered (1,-1). No need to 
-- worry about scaling the BoundingBox
--

psUnitTranslation :: (PtSize u, Ord u) => Picture u -> DPicture
psUnitTranslation = fmap psDouble



svgPageTranslation :: (PtSize u, Ord u) => Picture u -> DPicture
svgPageTranslation pic = 
   scale 1 (-1) (trivPic pic)

trivPic :: PtSize u => Picture u -> DPicture 
trivPic (Leaf lc ones)      = Leaf (trivLocale lc) (fmap trivPrim ones)
trivPic (Picture lc ones)   = Picture (trivLocale lc) (fmap trivPic ones)

trivLocale :: PtSize u => Locale u -> DLocale 
trivLocale (bb, dtrafos) = (fmap psDouble bb, dtrafos)


trivPrim :: PtSize u => Primitive u -> DPrimitive
trivPrim (PPath a pp)     = PPath a (fmap psDouble pp)
trivPrim (PLabel a lbl)   = PLabel a (trivLabel lbl)
trivPrim (PEllipse a ell) = PEllipse a (trivEllipse ell)
trivPrim (PContext a chi) = PContext a (trivPrim chi)
trivPrim (PSVG a chi)     = PSVG a (trivPrim chi)
trivPrim (PGroup ones)    = PGroup $ fmap trivPrim ones
trivPrim (PClip pp chi)   = PClip (fmap psDouble pp) (trivPrim chi)


trivLabel :: PtSize u => PrimLabel u -> DPrimLabel
trivLabel (PrimLabel txt ctm) = 
    PrimLabel (trivLabelBody txt) (trivPrimCTM ctm)

trivEllipse :: PtSize u => PrimEllipse u -> DPrimEllipse
trivEllipse (PrimEllipse hw hh ctm) = 
    PrimEllipse (psDouble hw) (psDouble hh) (trivPrimCTM ctm)

trivLabelBody :: PtSize u => LabelBody u -> DLabelBody
trivLabelBody (StdLayout esc) = StdLayout esc
trivLabelBody (KernTextH xs)  = KernTextH $ map trivKerningChar xs
trivLabelBody (KernTextV xs)  = KernTextV $ map trivKerningChar xs

trivKerningChar :: PtSize u => KerningChar u -> DKerningChar
trivKerningChar (u,esc) = (psDouble u, esc)


-- Negate the y scaling to flip the image.
--
trivPrimCTM :: PrimCTM -> PrimCTM
trivPrimCTM (PrimCTM dx dy sx sy theta) = PrimCTM dx dy sx (negate sy) theta

