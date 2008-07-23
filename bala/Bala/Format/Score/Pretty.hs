--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Format.Score.Pretty
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Pretty printer for Score format
--
--------------------------------------------------------------------------------

module Bala.Format.Score.Pretty where

import Bala.Base (afficher)
import Bala.Format.Score.Datatypes

import Text.PrettyPrint.Leijen


tagint i = let s = show i; l = length s in
  if l < 5 then text (replicate (5-l) '0' ++ s) else text s 

instance Pretty ScTag where
  pretty (ScTag i) = group $ braces (tagint i)

instance Pretty ScScore where
  pretty (ScScore xs) = fillSep (map pretty xs)

  
instance Pretty ScGlyph where
  pretty (ScNote tag pn oct dur)  = group $ 
      text (afficher pn) <> pretty tag <> int oct <> char '/' <> double dur
      
  pretty (ScRest tag dur)         = group $
      char 'R' <> pretty tag <> char '/' <> double dur 

  pretty (ScGroup tag typ xs)     = brackets $ 
      pretty tag <> char ':' <> groupdesc typ <+> hsep (map pretty xs)    
      
groupdesc :: ScGroupType -> Doc
groupdesc ScBeam        = text "beam"
groupdesc ScChord       = text "chord"
groupdesc ScGraceNotes  = text "grace_notes"


      