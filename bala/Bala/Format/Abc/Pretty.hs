--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Format.Abc.Pretty
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Pretty printer for ABC format
-- |
--------------------------------------------------------------------------------

module Bala.Format.Abc.Pretty where

import Bala.Format.Abc.Datatypes

import Text.PrettyPrint.Leijen

ppAbcFile :: AbcFile -> Doc
ppAbcFile = vsep . map pretty


instance Pretty AbcFileElement where
  pretty (AbcTuneElement (header,music))
      = pretty header <$> vsep (map pretty music)
  pretty (AbcTexCommandElement cmd)   = text cmd
  pretty (AbcFileFieldsElement xs)    = vsep (map pretty xs)
    
instance Pretty AbcHeader where
  pretty (AbcHeader fn fts ofs fk) 
      = vsep (map pretty ((fn:fts) ++ ofs)) <$> pretty fk
                                              
instance Pretty Field where
  pretty (FieldFile str)           = ppField 'F' (text str)
  pretty (FieldNumber i)           = ppField 'X' (int i)
  pretty (FieldTitle str)          = ppField 'T' (text str)
  pretty (FieldArea str)           = ppField 'A' (text str)
  pretty (FieldBook str)           = ppField 'B' (text str)
  pretty (FieldComposer str)       = ppField 'C' (text str)
  pretty (FieldDiscography str)    = ppField 'D' (text str)
  pretty (FieldElemskip str)       = ppField 'E' (text str)
  pretty (FieldGroup str)          = ppField 'G' (text str)
  pretty (FieldHistory ss)         = ppField 'H' (vsep $ map text ss)
  pretty (FieldInformation str)    = ppField 'I' (text str)
  pretty (FieldDefaultLength snl)  = ppField 'L' (ppFrac snl)
  pretty (FieldMeter meter)        = ppField 'M' (pretty meter)
  pretty (FieldNotes str)          = ppField 'N' (text str)
  pretty (FieldOrigin str)         = ppField 'O' (text str)
  pretty (FieldParts parts)        = ppField 'P' (hsep $ map pretty parts)
  pretty (FieldTempo tempo)        = ppField 'Q' (pretty tempo)
  pretty (FieldRhythm str)         = ppField 'R' (text str)
  pretty (FieldSource str)         = ppField 'S' (text str)
  pretty (FieldTranscrNotes str)   = ppField 'Z' (text str)
  pretty (FieldKey key)            = ppField 'K' (pretty key)
  pretty (FieldPart part)          = ppField 'P' (pretty part)
  pretty (FieldWords str)          = ppField 'W' (text str)


ppField :: Char -> Doc -> Doc
ppField ch doc = text [ch,':'] <+> doc

ppFrac :: (Int,Int) -> Doc
ppFrac (a,b) = int a <> char '/' <> int b
            
instance Pretty Key where
  pretty (Key keyspec)             = ppKeySpec keyspec
  pretty HighlandNoKey             = text "HP" 
  pretty HighlandMixolydian        = text "Hp"

ppKeySpec :: KeySpec -> Doc
ppKeySpec (kn, oms, []) = ppKeyNote kn <> maybe empty ppModeSpec oms
ppKeySpec (kn, oms, xs) = ppKeyNote kn <> maybe empty ppModeSpec oms <+> pretty xs

ppKeyNote :: KeyNote -> Doc 
ppKeyNote (bn, oac) = pretty bn <> maybe empty pretty oac


ppModeSpec :: ModeSpec -> Doc
ppModeSpec (m, str) = pretty m <> text str
  
instance Pretty KeyAccidental where
  pretty KeySharp                  = char '#' 
  pretty KeyFlat                   = char 'b'


instance Pretty Mode where
  pretty ModeMinor                 = char 'm'
  pretty ModeMajor                 = text "maj"
  pretty ModeLydian                = text "lyd"
  pretty ModeIonian                = text "ion"
  pretty ModeMixolydian            = text "mix"
  pretty ModeDorian                = text "dor"
  pretty ModeAeolian               = text "aeo"
  pretty ModePhrygian              = text "phr"
  pretty ModeLocrian               = text "loc"
  
instance Pretty Meter where
  pretty (Meter (i,j))             = int i <> char '/' <> int j
  pretty MeterCommonTime           = char 'C'
  pretty MeterCutTime              = text "C|"

  
instance Pretty Tempo where
  pretty (Tempo i)                 = int i
  pretty (TempoC nl i)             = char 'C' <> pretty nl <> equals <> int i
  pretty (TempoAbsolute snl i)     = pretty snl <> equals <> int i
    
      
instance Pretty Part where
  pretty (PartTree i ps)
      | i == 0                        = parens (hcat $ map pretty ps)
      | otherwise                     = int i <> parens (hcat $ map pretty ps)
      
  pretty (PartElem i c)            
      | i == 0                        = char c
      | otherwise                     = int i <> char c
      
      
instance Pretty AbcLine where
  pretty (Elements elems)          = hcat (map pretty elems)
  pretty (MidTexCommand cmd)       = text cmd
  pretty (MidTuneField fld)        = pretty fld
    
instance Pretty Element where
  pretty (NoteElement nelem)        = ppNoteElem nelem
  pretty (TupletElement tupspec ns) = pretty tupspec <> hcat (map ppNoteElem ns)
  pretty (Barline barline)          = pretty barline
  pretty (NthRepeat repmark)        = pretty repmark
  pretty (Slur slur)                = pretty slur
  pretty Space                      = char ' '
  pretty (UserDefined str)          = undefined

ppNoteElem :: NoteElement -> Doc 
ppNoteElem (stem, opt_br) = ppNoteStem stem <> maybe empty pretty opt_br

ppNoteStem :: NoteStem -> Doc 
ppNoteStem (opt_gchord, opt_gracenotes, gs, ns) 
    = maybe empty pretty opt_gchord <> maybe empty (hcat . map pretty) opt_gracenotes
      <> hcat (map pretty gs) <> hcat (map pretty ns)
    
instance Pretty Note where
  pretty (Note nv onl ot)          = pretty nv <> maybe empty pretty onl 
                                                  <> maybe empty pretty ot 

instance Pretty NoteOrRest where
  pretty (Pitch pch)          = pretty pch
  pretty Rest                 = char 'z'      


instance Pretty PitchSpec where
  pretty (PitchSpec bn oa oom)         = prefix oa <> pretty bn <> suffix oom
    where prefix = maybe empty pretty
          suffix = maybe empty pretty

instance Pretty Octave where
  pretty (OctaveLow i)             = text $ replicate i ','
  pretty (OctaveHigh i)            = text $ replicate i '\''

                      
instance Pretty Accidental where
  pretty Sharp                     = char '^'
  pretty DoubleSharp               = text "^^"
  pretty Flat                      = char '_'
  pretty DoubleFlat                = text "__"
  pretty Natural                   = char '='


instance Pretty BrokenRhythm where
  pretty (DottedLeft i)            = text $ replicate i '>'
  pretty (DottedRight i)           = text $ replicate i '<'


instance Pretty Tie where
  pretty Tie                       = char '-'


instance Pretty Gracing where
  pretty Tilde                     = char '~' 
  pretty Stacatto                  = char '.'
  pretty DownBow                   = char 'v'
  pretty UpDown                    = char 'u'


instance Pretty GuitarChord where
  pretty (FormalChord bn oct obn) 
      = pretty bn <> maybe empty text oct 
                  <> maybe empty (\a -> char '/' <> pretty a) obn
  pretty (UninterpretedChord str) = dquotes $ text str
    
instance Pretty Barline where
  pretty BarSingle                = char '|'
  pretty BarDouble                = text "||"
  pretty BarThickThin             = text "[|"
  pretty BarThinThick             = text "|]"
  pretty RepeatLeft               = text ":|"
  pretty RepeatRight              = text "|:"
  pretty RepeatBoth               = text "::"
  
instance Pretty RepeatMark where
  pretty RepeatFirst               = text "[1"
  pretty RepeatSecond              = text "[2"
  pretty EndingFirst               = text "|1"
  pretty EndingSecond              = text ":|2"
  
instance Pretty Slur where
  pretty SlurBegin                 = char '(' 
  pretty SlurEnd                   = char ')'
  
            
  

  

  


    
  
  
  
