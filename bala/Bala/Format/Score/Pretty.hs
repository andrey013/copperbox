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

module Bala.Format.Score.Pretty (Pretty(..)) where


import Bala.Format.Score.Class
import Bala.Format.Score.Datatypes
import Bala.Format.Score.PolyDatatypes

import qualified Data.IntMap as IM
import Data.Sequence hiding (length, empty)
import Text.PrettyPrint.Leijen

intPlex i = let s = show i in (s,length s)

tagint i = let (s,l) = intPlex i in
  if l < 5 then text (replicate (5-l) '0' ++ s) else text s 

sepSeq op sq = case viewl sq of
    EmptyL    -> empty
    e :< se   -> trav (pretty e) (viewl se)
  where
    trav doc EmptyL    = doc
    trav doc (e :< se) = trav (doc `op` pretty e) (viewl se)
    
{-
instance Pretty ScTag where
  pretty (ScTag i) = group $ braces (tagint i)
-}

instance (Printable pch, Printable dur) => Pretty (ScScore pch dur) where
  pretty (ScScore sb) = sepSeq (<$>) sb

instance (Printable pch, Printable dur) => Pretty (ScPart pch dur) where
  pretty (ScPart i refs sm) = prefix i <$> text ":part " <> int i 
                                       <$> sepSeq (<$>) sm 
                                       <$> pprefs (getPolyRefs refs)
    where 
      prefix i  = let l = snd $ intPlex i in text $ replicate (l+6) '-'                                  
      pprefs    = indent 2 . vsep . map fn . IM.toAscList
      fn (i,sm) = char '#' <> int i <+> sepSeq (</>) sm
  
{-  
instance (Printable pch, Printable dur) => Pretty (ScPoly pch dur) where
  pretty (ScPolyM m)  = pretty m
  pretty (ScPolyRef xs)  = encloseSep lbracket rbracket (char '-') $ 
                                  map int xs
-}  
  
instance (Printable pch, Printable dur) => Pretty (ScMeasure pch dur) where
  pretty (ScMeasure i sr se) = text "|:" <> int i <+> pprefs sr
                                         <+> sepSeq (</>) se
    where pprefs sr  = enclose lbracket rbracket $ 
                                  sepSeq (\a b -> a <> char '-' <> b) sr                                         
  
instance (Printable pch, Printable dur) => Pretty (ScGlyph pch dur) where
  pretty (ScNote pch dur)         = ppNote pch dur      
  pretty (ScRest dur)             = ppAltRest 'R' dur
  pretty (ScSpacer dur)           = ppAltRest 'S' dur      
  pretty (ScGroup typ xs)         = ppGroup (groupdesc typ)  xs    


--  pretty (ScTaggedGlyph tag)  = group $ 
--      char 'X' <> pretty tag


ppNote pch dur = group $ 
      pretty pch <> char '/' <> (text $ stringrep dur)

ppAltRest ch dur = group $
      char ch <> char '/' <> (text $ stringrep dur)      

ppGroup d xs = brackets $ 
      char ':' <> d <+> hsep (map pretty xs) 


      
groupdesc :: ScGroupType -> Doc
groupdesc ScBeam        = text "beam"
groupdesc ScChord       = text "chord"
groupdesc ScGraceNotes  = text "grace_notes"


instance (Printable pch) => Pretty (ScPitch pch) where
  pretty (ScPitch a)     = text $ stringrep a

-- 

instance (Printable pch, Printable dur) => Pretty (PScScore pch dur) where
  pretty (PScScore sb) = sepSeq (<$>) sb

instance (Printable pch, Printable dur) => Pretty (PScPart pch dur) where
  pretty (PScPart i se) = text ":part " <> int i <$> sepSeq (<$>) se


instance (Printable pch, Printable dur) => Pretty (PScPolyUnit pch dur) where
  pretty (PScPolyUnit [x]) = pretty x
  pretty (PScPolyUnit xs)  = encloseSep l r s (map pretty xs)
    where
      l = text "<< " 
      r = text " >>"
      s = text " \\\\ " 

instance (Printable pch, Printable dur) => Pretty (PScSegment pch dur) where
  pretty (PScSegment se) = sepSeq (</>) se

instance (Printable pch, Printable dur) => Pretty (PScMeasure pch dur) where
  pretty (PScMeasure i v se) = text "|:" <> int i <+> char 'v' <> int v 
                                         <+> sepSeq (</>) se

pgroupdesc :: PScGroupType -> Doc
pgroupdesc PScBeam        = text "beam"
pgroupdesc PScChord       = text "chord"
pgroupdesc PScGraceNotes  = text "grace_notes"


instance (Printable pch, Printable dur) => Pretty (PScGlyph pch dur) where
  pretty (PScNote pch dur)         = ppNote pch dur      
  pretty (PScRest dur)             = ppAltRest 'R' dur
  pretty (PScSpacer dur)           = ppAltRest 'S' dur      
  pretty (PScGroup typ xs)         = ppGroup (pgroupdesc typ)  xs  
  

instance (Printable pch) => Pretty (PScPitch pch) where
  pretty (PScPitch a)     = text $ stringrep a
      