

--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.PrettyInstances
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- Debugging helpers - pretty printers of various internal formats.
--
--------------------------------------------------------------------------------


module HNotate.PrettyInstances where

import HNotate.CommonUtils
import HNotate.NoteList
import HNotate.TemplateDatatypes

import qualified Data.Foldable as F
import Text.PrettyPrint.Leijen


                              
--------------------------------------------------------------------------------
-- pretty printing

hshComment :: Doc -> Doc
hshComment = enclose (text "{-# ") (text " #-}")

instance (Pretty e) => Pretty (ScNoteList e) where
  pretty (ScNoteList se) = genPunctuateSeq pretty line se



instance (Pretty e) => Pretty (ScBlock e) where
  pretty (ScSingleBlock i e) = measureNumber i
                                         <$> indent 4 (pretty e)
  pretty (ScPolyBlock i se)  = 
      measureNumber i <$> indent 4 (encloseSep (text "<< ") 
                                               (text " >>") 
                                               (text " // ")
                                               (map pretty $ F.toList se))

measureNumber :: Int -> Doc
measureNumber i = text "|:" <>  int i


instance (Pretty e) => Pretty (ScMeasure e) where
  pretty (ScMeasure se) = genPunctuateSeq pretty space se


instance Pretty ScoreGlyph where
  pretty (SgNote pch dur)         = pretty pch <> durationSuffix dur
  pretty (SgRest dur)             = char 'r' <> durationSuffix dur
  pretty (SgSpacer dur)           = char 's' <> durationSuffix dur
  pretty (SgChord ps dur)         = (brackets $ genPunctuateSeq pretty space ps) 
                                      <> durationSuffix dur
  pretty (SgGraceNotes es)        = braces $ genPunctuateSeq pretty space es

  pretty (SgBeamStart)            = text "[."
  pretty (SgBeamEnd)              = text ".]" <> line
  pretty (SgTie)                  = text "~~"
                

durationSuffix :: Pretty drn => drn -> Doc
durationSuffix d = char '\'' <> pretty d 


intPlex i = let s = show i in (s,length s)

tagint len i = let (s,l) = intPlex i in
  if l < len then text (replicate (len -l) '0' ++ s) else text s
  
--------------------------------------------------------------------------------
-- Template files

ppPos :: SrcPos -> Doc 
ppPos (SrcPos l c _) = pretty $ (l,c)

instance Pretty TextualView where
  pretty (TextualView se) = F.foldl (\d e -> d <> pretty e) empty se <> line


instance Pretty TextElement where
  pretty (SourceText str)     = string str
  pretty (MetaMark idx pos d) = hshComment (ppPos pos <+> int idx <+> pretty d)

  
instance Pretty MetaDirective where
  pretty (MetaOutput scm idx) = pretty scm <> colon <> text idx
  
instance Pretty OutputScheme where
  pretty LyRelative = text "relative" 
  pretty AbcDefault = text "default"
  
    
 
                                                
