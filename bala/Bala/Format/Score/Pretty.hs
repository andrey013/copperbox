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
import Bala.Format.Score.Class
import Bala.Format.Score.Datatypes

import qualified Data.Map as Map
import Data.Sequence hiding (length, empty)
import Text.PrettyPrint.Leijen

integerPlex i = let s = show i in (s,length s)

tagint i = let (s,l) = integerPlex i in
  if l < 5 then text (replicate (5-l) '0' ++ s) else text s 

sepSeq op sq = case viewl sq of
    EmptyL    -> empty
    e :< se   -> trav (pretty e) (viewl se)
  where
    trav doc EmptyL    = doc
    trav doc (e :< se) = trav (doc `op` pretty e) (viewl se)
    

instance Pretty ScTag where
  pretty (ScTag i) = group $ braces (tagint i)

instance (Printable pch, Printable dur) => Pretty (ScScore pch dur) where
  pretty (ScScore sb) = sepSeq (<$>) sb

instance (Printable pch, Printable dur) => Pretty (ScPart pch dur) where
  pretty (ScPart i refs sm) = prefix i <$> text ":part " <> integer i 
                                       <$> sepSeq (<$>) sm 
                                       <$> pprefs (getRefs refs)
    where 
      prefix i  = let l = snd $ integerPlex i in text $ replicate (l+6) '-'                                  
      pprefs    = indent 2 . vsep . map fn . Map.toAscList
      fn (i,sm) = char '#' <> integer i <+> sepSeq (</>) sm
  
{-  
instance (Printable pch, Printable dur) => Pretty (ScPoly pch dur) where
  pretty (ScPolyM m)  = pretty m
  pretty (ScPolyRef xs)  = encloseSep lbracket rbracket (char '-') $ 
                                  map integer xs
-}  
  
instance (Printable pch, Printable dur) => Pretty (ScMeasure pch dur) where
  pretty (ScMeasure i sr se) = text "|:" <> integer i <+> pprefs sr
                                         <+> sepSeq (</>) se
    where pprefs sr  = enclose lbracket rbracket $ 
                                  sepSeq (\a b -> a <> char '-' <> b) sr                                         
  
instance (Printable pch, Printable dur) => Pretty (ScGlyph pch dur) where
  pretty (ScNote pch dur)  = group $ 
      pretty pch <> char '/' <> (text $ stringrep dur)
      
  pretty (ScRest dur)             = group $
      char 'R' <> char '/' <> (text $ stringrep dur)

  pretty (ScSpacer dur)           = group $
      char 'S' <> char '/' <> (text $ stringrep dur)
      
  pretty (ScGroup typ xs)         = brackets $ 
      char ':' <> groupdesc typ <+> hsep (map pretty xs)    

--  pretty (ScTaggedGlyph tag)  = group $ 
--      char 'X' <> pretty tag

      
groupdesc :: ScGroupType -> Doc
groupdesc ScBeam        = text "beam"
groupdesc ScChord       = text "chord"
groupdesc ScGraceNotes  = text "grace_notes"


instance (Printable pch) => Pretty (ScPitch pch) where
  pretty (ScPitch a)     = text $ stringrep a
      