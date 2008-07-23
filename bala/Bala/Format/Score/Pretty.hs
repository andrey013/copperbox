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

instance Pretty ScScore where
  pretty (ScScore sb) = sepSeq (<$>) sb

instance Pretty ScPart where
  pretty (ScPart i sb) = prefix i <$> text ":part " <> integer i 
                                  <$> sepSeq (<$>) sb
    where prefix i = let l = snd $ integerPlex i
                     in text $ replicate (l+6) '-'                                  


instance Pretty ScBar where
  pretty (ScBar i se) = text "|:" <> integer i <+> sepSeq (</>) se
  
instance Pretty ScGlyph where
  pretty (ScNote pn oct dur)  = group $ 
      text (afficher pn) <> int oct <> char '/' <> double dur
      
  pretty (ScRest dur)             = group $
      char 'R' <> char '/' <> double dur 

  pretty (ScGroup typ xs)     = brackets $ 
      char ':' <> groupdesc typ <+> hsep (map pretty xs)    

  pretty (ScRef tag )             = group $ 
      char 'X' <> pretty tag
      
groupdesc :: ScGroupType -> Doc
groupdesc ScBeam        = text "beam"
groupdesc ScChord       = text "chord"
groupdesc ScGraceNotes  = text "grace_notes"


      