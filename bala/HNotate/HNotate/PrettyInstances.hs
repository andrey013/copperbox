

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
import HNotate.Env
import HNotate.MusicRepDatatypes
import HNotate.NoteListDatatypes
import HNotate.OnsetQueue
import HNotate.TemplateDatatypes

import Data.Char (toLower)
import qualified Data.Foldable as F
import qualified Data.Map as Map
import Data.Ratio
import Text.PrettyPrint.Leijen


instance (Integral a, Pretty a) => Pretty (Ratio a) where
  pretty r = pretty (numerator r) <> char '%' <> pretty (denominator r)

--------------------------------------------------------------------------------
-- Env

instance Pretty OutputFormat where
  pretty Output_Abc           = text "Abc"
  pretty Output_LilyPond      = text "LilyPond"  


instance Pretty Env where
  pretty e  = text "Env:" <$> 
      eline "output_format"     output_format     pretty          <$> 
      eline "current_key"       current_key       pretty          <$>
      eline "label_set"         label_set         pretty          <$> 
      eline "current_meter"     current_meter     pretty          <$> 
      eline "meter_pattern"     meter_pattern     ppMeterPattern  <$>
      eline "measure_length"    measure_length    ppDuration      <$>
      eline "unit_note_length"  unit_note_length  ppDuration      <$>
      eline "relative_pitch"    relative_pitch    pretty          <$>
      eline "partial_measure"   partial_measure   optPM           <$>
      eline "cadenza"           cadenza           pretty          <$>
      eline "bar_number_check"  bar_number_check  pretty          <$>
      eline "score_comment"     score_comment     ppfun  
                  
    where 
      eline :: String -> (Env -> a) -> (a -> Doc) -> Doc
      eline s f pp = fill 20 (text s) <> colon <+> pp (f e)  
      
      ppfun        = const $ text "<fun>"
      
      optPM (Just d)  = ppDuration d   
      optPM Nothing   = text "unspecified - full bar"             

  

--------------------------------------------------------------------------------
-- NoteListDatatypes


instance (Pretty e) => Pretty (NoteListF e) where
  pretty (NoteList se)        = genPunctuateSeq pretty line se



instance (Pretty e) => Pretty (BlockF e) where
  pretty (SingleBlock i e)    = measureNumber i <$> indent 4 (pretty e)
  pretty (PolyBlock i se)     = 
      measureNumber i <$> indent 4 (encloseSep (text "<< ") 
                                               (text " >>") 
                                               (text " // ")
                                               (map pretty $ F.toList se))

measureNumber :: Int -> Doc
measureNumber i = text "|:" <>  int i


instance (Pretty e) => Pretty (BarF e) where
  pretty (Bar se)             = genPunctuateSeq pretty space se


instance Pretty Glyph where
  pretty (Note pch dur)       = pretty pch <> durationSuffix dur
  pretty (Rest dur)           = char 'r' <> durationSuffix dur
  pretty (Spacer dur)         = char 's' <> durationSuffix dur
  pretty (Chord ps dur)       = (brackets $ genPunctuateSeq pretty space ps) 
                                      <> durationSuffix dur
  pretty (GraceNotes es)      = braces $ genPunctuateSeq pretty space es

  pretty (BeamStart)          = text "[."
  pretty (BeamEnd)            = text ".]" <> line
  pretty (Tie)                = text "~~"
                

durationSuffix :: Duration -> Doc
durationSuffix d = char '\'' <> ppDuration d 



--------------------------------------------------------------------------------
-- MusicRepDatatypes
ppMeterPattern (ns,d) = 
    hcat (punctuate (char '+') $ map  pretty ns) <> char '/'<> ppDuration d

instance Pretty Meter where
  pretty (TimeSig n d)        = int n <> char '/' <> int d
  pretty CommonTime           = char 'C'
  pretty CutTime              = text "C|"


instance Pretty PitchLabel where
  pretty (PitchLabel l a)     = pretty l <> pretty a

instance Pretty Key where
  pretty (Key pl mode)        = pretty pl <+> char '\\' <> pretty mode

instance Pretty Mode where
  pretty = text . fmap toLower . show
  
  
instance Pretty LabelSet where
  pretty (LabelSet mp) = 
      hcat $ punctuate (char '-') (map (pretty . snd) $ Map.toAscList mp) 



instance Pretty a => Pretty (OnsetQueue a) where
  pretty = foldlOnsetQueue fn empty
    where fn d (i,xs) = d <$> int i <+> text ":+" <+> list (map pretty xs)    


--------------------------------------------------------------------------------
-- TemplateDatatypes

ppPos :: SrcPos -> Doc 
ppPos (SrcPos l c _) = pretty $ (l,c)

  
instance Pretty OutputScheme where
  pretty OutputRelative       = text "relative" 
  pretty OutputDefault        = text "default"
  
instance Pretty Expr where
  pretty (Expr t es)          = pretty t <$> indent 2 
                                    (braces $ hsep $ map pretty es)
  
instance Pretty Term where
  pretty (Let bind)                   
        = text "let" <+> pretty bind
  pretty (OutputDirective oscm name)  
      = text "#output" <> (maybe empty ((empty <+>) . pretty) oscm) <+> text name  
     

instance Pretty Binding where
  pretty (LetCadenza b)       = equation "cadenza"        (pretty b)
  pretty (LetKey key)         = equation "key"            (pretty key)
  pretty (LetMeter meter)     = equation "time"           (pretty meter)
  pretty (LetMeterPattern mp) = equation "meter_pattern"  (ppMeterPattern mp)
  pretty (LetPartial d)       = equation "partial"        (ppDuration d)
  pretty (LetRelativePitch p) = equation "relative"       (pretty p)
  pretty (LetNone)            = text "#NONE" 

equation s e = ppcmd s <+> equals <+> e      
ppcmd = text . ('\\':) 



 
foldrMapE op f = foldr (op `onl` f)

