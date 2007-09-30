
module Sound.Abc.Pretty where

import Sound.Abc.Datatypes

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
                                              
instance Pretty AbcField where
  pretty (AbcFileField str)           = ppField 'F' (text str)
  pretty (AbcNumberField i)           = ppField 'X' (int i)
  pretty (AbcTitleField str)          = ppField 'T' (text str)
  pretty (AbcAreaField str)           = ppField 'A' (text str)
  pretty (AbcBookField str)           = ppField 'B' (text str)
  pretty (AbcComposerField str)       = ppField 'C' (text str)
  pretty (AbcDiscographyField str)    = ppField 'D' (text str)
  pretty (AbcElemskipField str)       = ppField 'E' (text str)
  pretty (AbcGroupField str)          = ppField 'G' (text str)
  pretty (AbcHistoryField ss)         = ppField 'H' (vsep $ map text ss)
  pretty (AbcInformationField str)    = ppField 'I' (text str)
  pretty (AbcDefaultLengthField snl)  = ppField 'L' (pretty snl)
  pretty (AbcMeterField meter)        = ppField 'M' (pretty meter)
  pretty (AbcNotesField str)          = ppField 'N' (text str)
  pretty (AbcOriginField str)         = ppField 'O' (text str)
  pretty (AbcPartsField parts)        = ppField 'P' (hsep $ map pretty parts)
  pretty (AbcTempoField tempo)        = ppField 'Q' (pretty tempo)
  pretty (AbcRhythmField str)         = ppField 'R' (text str)
  pretty (AbcSourceField str)         = ppField 'S' (text str)
  pretty (AbcTranscrNotesField str)   = ppField 'Z' (text str)
  pretty (AbcKeyField key)            = ppField 'K' (pretty key)
  pretty (AbcPartField part)          = ppField 'P' (pretty part)
  pretty (AbcWordsField str)          = ppField 'W' (text str)


ppField :: Char -> Doc -> Doc
ppField ch doc = text [ch,':'] <$> doc
            
instance Pretty AbcKey where
  pretty (AbcKey keyspec)             = pretty keyspec
  pretty AbcHighlandNoKey             = text "HP" 
  pretty AbcHighlandMixolydian        = text "Hp"
  
instance Pretty AbcKeyAccidental where
  pretty AbcSharpKey                  = char '#' 
  pretty AbcFlatKey                   = char 'b'


instance Pretty AbcMode where
  pretty AbcMinorMode                 = char 'm'
  pretty AbcMajorMode                 = text "maj"
  pretty AbcLydianMode                = text "lyd"
  pretty AbcIonianMode                = text "ion"
  pretty AbcMixolydianMode            = text "mix"
  pretty AbcDorianMode                = text "dor"
  pretty AbcAeolianMode               = text "aeo"
  pretty AbcPhrygianMode              = text "phr"
  pretty AbcLocrianMode               = text "loc"
  
instance Pretty AbcMeter where
  pretty (AbcMeter i j)               = int i <> char '/' <> int j
  pretty AbcCommonTimeMeter           = char 'C'
  pretty AbcCutTimeMeter              = text "C|"

  
instance Pretty AbcTempo where
  pretty (AbcTempo i)                 = int i
  pretty (AbcCTempo nl i)             = char 'C' <> pretty nl <> equals <> int i
  pretty (AbcAbsoluteTempo snl i)     = pretty snl <> equals <> int i
    
      
instance Pretty AbcPart where
  pretty (AbcPartTree i ps)
      | i == 0                        = parens (hcat $ map pretty ps)
      | otherwise                     = int i <> parens (hcat $ map pretty ps)
      
  pretty (AbcPartElem i c)            
      | i == 0                        = char c
      | otherwise                     = int i <> char c
      
      
instance Pretty AbcLine where
  pretty (AbcElements elems)          = hcat (map pretty elems)
  pretty (AbcMidTexCommand cmd)       = text cmd
  pretty (AbcMidTuneField fld)        = pretty fld
    
instance Pretty AbcElement where
  pretty (AbcNoteElement nelem)         = ppNoteElem nelem
  pretty (AbcTupletElement tupspec ns)  = pretty tupspec <> hcat (map ppNoteElem ns)
  pretty (AbcBarlineElement barline)    = pretty barline
  pretty (AbcRepeatElement repmark)     = pretty repmark
  pretty (AbcSlurElement slur)          = pretty slur
  pretty AbcSpaceElement                = char ' '
  pretty (AbcUserDefinedElement str)    = undefined

ppNoteElem :: Note_element -> Doc 
ppNoteElem (stem, opt_br) = ppNoteStem stem <> maybe empty pretty opt_br

ppNoteStem (opt_gchord, opt_gracenotes, gs, ns) 
    = maybe empty pretty opt_gchord <> maybe empty (hcat . map pretty) opt_gracenotes
      <> hcat (map pretty gs) <> hcat (map pretty ns)
    
instance Pretty AbcNote where
  pretty (AbcNote nv onl ot)          = pretty nv <> maybe empty pretty onl 
                                                  <> maybe empty pretty ot 

instance Pretty AbcValue where
  pretty (AbcPitchValue pch)          = pretty pch
  pretty AbcRest                      = char 'z'      


instance Pretty AbcPitch where
  pretty (AbcPitch bn oa oom)         = prefix oa <> pretty bn <> suffix oom
    where prefix = maybe empty pretty
          suffix = maybe empty (\a -> char '/' <> pretty a)

instance Pretty AbcOctave where
  pretty (AbcLowOctave i)             = text $ replicate i '\''
  pretty (AbcHighOctave i)            = text $ replicate i ','

                      
instance Pretty AbcAccidental where
  pretty AbcSharp                     = char '^'
  pretty AbcDoubleSharp               = text "^^"
  pretty AbcFlat                      = char '_'
  pretty AbcDoubleFlat                = text "__"
  pretty AbcNatural                   = char '='


instance Pretty AbcBrokenRhythm where
  pretty (AbcDottedLeft i)            = text $ replicate i '>'
  pretty (AbcDottedRight i)           = text $ replicate i '<'


instance Pretty AbcTie where
  pretty AbcTie                       = char '-'


instance Pretty AbcGracing where
  pretty AbcTilde                     = char '~' 
  pretty AbcStacatto                  = char '.'
  pretty AbcDownBow                   = char 'v'
  pretty AbcUpDown                    = char 'u'


instance Pretty AbcGuitarChord where
  pretty (AbcFormalChord bn oct obn) 
      = pretty bn <> maybe empty text oct 
                  <> maybe empty (\a -> char '/' <> pretty a) obn
  pretty (AbcUninterpretedChord str) = dquotes $ text str
    
instance Pretty AbcBarline where
  pretty AbcSingleBar                 = char '|'
  pretty AbcDoubleBar                 = text "||"
  pretty AbcThick_ThinBar             = text "[|"
  pretty AbcThin_ThickBar             = text "|]"
  pretty AbcLeftRepeat                = text ":|"
  pretty AbcRightRepeat               = text "|:"
  pretty AbcBothRepeat                = text "::"
  
instance Pretty AbcRepeatMark where
  pretty AbcFirstRepeat               = text "[1"
  pretty AbcSecondRepeat              = text "[2"
  pretty AbcFirstEnding               = text "|1"
  pretty AbcSecondEnding              = text ":|2"
  
instance Pretty AbcSlur where
  pretty AbcBeginSlur                 = char '(' 
  pretty AbcEndSlur                   = char ')'
  
            
  

  

  


    
  
  
  
