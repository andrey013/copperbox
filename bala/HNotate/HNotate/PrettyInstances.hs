

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
import HNotate.Duration
import HNotate.MusicRepDatatypes
import HNotate.NoteListDatatypes
import HNotate.TemplateDatatypes

import qualified Data.Foldable as F
import qualified Data.Map as Map
import Data.Ratio
import Text.PrettyPrint.Leijen


ppDur :: Duration -> Doc
ppDur = pretty . printableDuration


--------------------------------------------------------------------------------
-- Music Rep 

instance Pretty PitchLabel where
  pretty (PitchLabel l a) = pretty l <> pretty a

instance Pretty LabelSet where
  pretty (LabelSet mp) = 
      hcat $ punctuate (char '-') (map (pretty . snd) $ Map.toAscList mp) 

                              
--------------------------------------------------------------------------------
-- pretty printing

hshComment :: Doc -> Doc
hshComment = enclose (text "{-# ") (text " #-}")

instance (Pretty e) => Pretty (NoteListF e) where
  pretty (NoteList se)      = genPunctuateSeq pretty line se



instance (Pretty e) => Pretty (BlockF e) where
  pretty (SingleBlock i e)  = measureNumber i <$> indent 4 (pretty e)
  pretty (PolyBlock i se)   = 
      measureNumber i <$> indent 4 (encloseSep (text "<< ") 
                                               (text " >>") 
                                               (text " // ")
                                               (map pretty $ F.toList se))

measureNumber :: Int -> Doc
measureNumber i = text "|:" <>  int i


instance (Pretty e) => Pretty (BarF e) where
  pretty (Bar se)                 = genPunctuateSeq pretty space se


instance Pretty Glyph where
  pretty (Note pch dur)           = pretty pch <> durationSuffix dur
  pretty (Rest dur)               = char 'r' <> durationSuffix dur
  pretty (Spacer dur)             = char 's' <> durationSuffix dur
  pretty (Chord ps dur)           = (brackets $ genPunctuateSeq pretty space ps) 
                                      <> durationSuffix dur
  pretty (GraceNotes es)          = braces $ genPunctuateSeq pretty space es

  pretty (BeamStart)              = text "[."
  pretty (BeamEnd)                = text ".]" <> line
  pretty (Tie)                    = text "~~"
                

durationSuffix :: Duration -> Doc
durationSuffix d = char '\'' <> ppDur d 


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
  pretty OutputRelative = text "relative" 
  pretty OutputDefault = text "default"
  
    
 
                                                
