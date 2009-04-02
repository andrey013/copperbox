{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.Staff
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- Rendering is done in /lines/ of music a line is equivalent to a voice
-- in Abc or a staff in LilyPond. 
-- We use the LilyPond term /staff/ for a line.
--
--------------------------------------------------------------------------------



module HNotate.Staff where

import HNotate.Cardinal

import Text.PrettyPrint.Leijen

newtype Staff a = Staff { getStaff :: [Overlay a] }
  deriving (Show)

-- Follow the Abc style when voice overlays are grouped in whole bars.
type Overlay a         = Cardinal (Bar a)

type BeamGroup a = Cardinal a

data Bar a  = Bar [BeamGroup a] | TiedBar a [BeamGroup a]
  deriving (Show)              

instance Functor Bar where
  fmap f (Bar xs)       = Bar (fmap (fmap f) xs) 
  fmap f (TiedBar x xs) = TiedBar (f x) (fmap (fmap f) xs) 
  
  
instance Pretty a => Pretty (Staff a) where
  pretty (Staff xs) = vsep $ map ppOverlay xs
  
ppOverlay :: Pretty a => Overlay a -> Doc
ppOverlay (Single a) = text "BAR:" <+> pretty a
ppOverlay (Multi xs) = text "BAR:" <+> indent 6 (vsep $ map pretty xs) 


                        
instance Pretty a => Pretty (Bar a) where
  pretty (Bar bs)       = hsep $ map ppBeamGroup bs
  pretty (TiedBar a bs) = char '~' <+> pretty a <+> hsep (map ppBeamGroup bs)
  
ppBeamGroup :: Pretty a => BeamGroup a -> Doc
ppBeamGroup (Single a) = pretty a
ppBeamGroup (Multi xs) = braces . hsep $ map pretty xs

 

