{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.StructuralDatatypes
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- Datatypes for the /structural/ organisation of scores...
--
--------------------------------------------------------------------------------



module HNotate.StructuralDatatypes where

import HNotate.Cardinal

import Text.PrettyPrint.Leijen

newtype Section a = Section { getSection :: [Overlay a] }
  deriving (Show)

-- Follow the Abc style when voice overlays are grouped in whole bars.
type Overlay a         = Cardinal (Bar a)

type BeamGroup a = Cardinal a

data Bar a  = Bar [BeamGroup a] | TiedBar a [BeamGroup a]
  deriving (Show)              


data Compo a = Literal a   -- 
             | Compo a :->- Compo a
             | Repeated (Compo a)


repeated :: Compo a -> Compo a
repeated = Repeated

infixl 5 ->-
(->-) :: Compo a -> Compo a -> Compo a
(->-) = (:->-)
--------------------------------------------------------------------------------
-- instances

instance Functor Bar where
  fmap f (Bar xs)       = Bar (fmap (fmap f) xs) 
  fmap f (TiedBar x xs) = TiedBar (f x) (fmap (fmap f) xs) 
  


--------------------------------------------------------------------------------
-- Pretty print
  
instance Pretty a => Pretty (Section a) where
  pretty (Section xs) = vsep $ map ppOverlay xs
  
ppOverlay :: Pretty a => Overlay a -> Doc
ppOverlay (Single a) = text "BAR:" <+> pretty a
ppOverlay (Multi xs) = text "BAR:" <+> indent 6 (vsep $ map pretty xs) 


                        
instance Pretty a => Pretty (Bar a) where
  pretty (Bar bs)       = hsep $ map ppBeamGroup bs
  pretty (TiedBar a bs) = char '~' <+> pretty a <+> hsep (map ppBeamGroup bs)
  
ppBeamGroup :: Pretty a => BeamGroup a -> Doc
ppBeamGroup (Single a) = pretty a
ppBeamGroup (Multi xs) = braces . hsep $ map pretty xs

 

