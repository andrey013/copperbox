{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

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
-- Debugging helpers - Pretty and Witness instances for 
-- internal datatypes.
--
--------------------------------------------------------------------------------


module HNotate.PrettyInstances where

import HNotate.CommonUtils
import HNotate.Duration
import HNotate.Env
import HNotate.MusicRepDatatypes
import HNotate.NotateMonad
import HNotate.NoteListDatatypes
import HNotate.OnsetQueue
import HNotate.TemplateDatatypes

import Data.Char (toLower)
import qualified Data.Foldable as F
import qualified Data.Map as Map
import Data.Ratio
import Text.PrettyPrint.Leijen
import Text.ParserCombinators.Parsec (ParseError)



--------------------------------------------------------------------------------
-- Witnesses for external types
instance Witness String where textrep = id

instance Witness Int where textrep = show
instance Witness Integer where textrep = show
instance (Integral a, Show a) => Witness (Ratio a) where textrep = show

instance (Witness a, Witness b) => Witness (Either a b) where
  textrep (Left a)  = "Left:\n"  ++ textrep a
  textrep (Right b) = "Right:\n" ++ textrep b
  
instance Witness ParseError where textrep = show

    

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
      eline "bar_length"        bar_length        ppDuration      <$>
      eline "unit_note_length"  unit_note_length  ppDuration      <$>
      eline "relative_pitch"    relative_pitch    pretty          <$>
      eline "anacrusis"         anacrusis         optAnacrusis    <$>
      eline "unmetered"         unmetered         pretty          <$>
      eline "bar_number_check"  bar_number_check  pretty          <$>
      eline "score_comment"     score_comment     ppfun  
                  
    where 
      eline :: String -> (Env -> a) -> (a -> Doc) -> Doc
      eline s f pp = fill 20 (text s) <> colon <+> pp (f e)  
      
      ppfun        = const $ text "<fun>"
      
      optAnacrusis (Just d)  = ppDuration d   
      optAnacrusis Nothing   = text "none"             

instance Witness Env where textrep = wpretty . pretty

--------------------------------------------------------------------------------
-- NoteListDatatypes


instance (Pretty e) => Pretty (NoteListF e) where
  pretty (NoteList se)        = genPunctuateSeq pretty line se

instance (Pretty e) => Witness (NoteListF e) where
  textrep = wpretty . pretty

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
  pretty (Key pl mode accs)   = pretty pl <+> char '\\' 
                                          <> pretty mode <> pp accs
    where
      pp [] = empty
      pp xs = hsep $ map pretty xs
  
instance Pretty Mode where
  pretty = text . fmap toLower . show
  
  
instance Pretty LabelSet where
  pretty (LabelSet mp) = 
      hcat $ punctuate (char '-') (map (pretty . snd) $ Map.toAscList mp) 



instance Pretty a => Pretty (OnsetQueue a) where
  pretty = foldlOnsetQueue fn empty
    where fn d (i,xs) = d <$> int i <+> text ":+" <+> list (map pretty xs)    


instance Pretty a => Witness (OnsetQueue a) where
  textrep = wpretty . pretty

--------------------------------------------------------------------------------
-- TemplateDatatypes

ppPos :: SrcPos -> Doc 
ppPos (SrcPos l c _) = pretty $ (l,c)

  
instance Pretty OutputScheme where
  pretty OutputRelative       = text "relative" 
  pretty OutputDefault        = text "default"


-- shared with LilyPond
instance Pretty MetaOutput where
  pretty (MetaOutput oscm name) = bananas body
    where body      = text "#output" <> colon <+> optscheme <+> text name
          optscheme = maybe empty pretty oscm


-- shared with LilyPond  
instance Pretty MetaBinding where
  pretty (MetaMeterPattern mp)  = metabind "meter_pattern" (pretty mp)
  pretty (MetaPartial d)        = metabind "partial" (pretty d)

metabind :: String -> Doc -> Doc  
metabind name d = bananas $ text ('~':name) <> colon <+> d  
  
instance Pretty AbcScore where
  pretty (AbcScore xs) = vsep $ map pretty xs

instance Witness AbcScore where textrep = wpretty . pretty


-- X field gives the Int
instance Pretty AbcTune where
  pretty (AbcTune n xs)         = 
      text "X:" <+> int n <+> indent 2 (list $ map pretty xs)

  
instance Pretty AbcExpr where
  pretty (AbcFieldBinding field)      = pretty field
  pretty (AbcMetaBinding mb)          = pretty mb
  pretty (AbcOutput mo)               = pretty mo
             
             
instance Pretty AbcField where
  pretty (AbcKey k)         = text "K:" <> pretty k
  pretty (AbcMeter m)       = text "M:" <> pretty m

  
    
instance Pretty LyScore where
  pretty (LyScore xs) = vsep $ map pretty xs
 
instance Witness LyScore where textrep = wpretty . pretty

  
instance Pretty LyExpr where
  pretty (LyCmdBinding cmd)   = pretty cmd
  pretty (LyMetaBinding mb)   = pretty mb 
  pretty (LyOutput mo)        = pretty mo
  pretty (LyNestExpr es)      = indent 2 $ list (map pretty es)
  


instance Pretty LyCommand where
  pretty (LyCadenza True)   = text "\\cadenzaOn"
  pretty (LyCadenza False)  = text "\\cadenzaOn"
  pretty (LyKey key)        = text "\\key"        <+> pretty key
  pretty (LyPartial d)      = text "\\partial"    <+> pretty d
  pretty (LyRelative p)     = text "\\relative"   <+> pretty p
  pretty (LyTime meter)     = text "\\time"       <+> pretty meter
  
    
  
instance Pretty Expr where
  pretty (Let bind e) = text "let" <+> pretty bind <+> text "in"
                          <$>  indent 2 (pretty e)   
  pretty (SDo out e)  = text "sdo" <+> pretty out <+> text "then" <+> pretty e   
  pretty (Do out)     = text "do" <+> pretty out <+> text "end"  
  pretty (Fork e1 e2) = text "fork" <+> 
                              nest 2 (text "<<" </> pretty e1 
                                                <$> pretty "//" 
                                                </> pretty e2)
                             <$> text ">>"
                                      
instance Witness [Expr] where
  textrep = wpretty . vsep . map pretty
        
instance Pretty OutputDirective where
  pretty (OutputDirective oscm name)  
      = text "#output" <> (maybe empty ((empty <+>) . pretty) oscm) <+> text name  
     

instance Pretty Binding where
  pretty (LetCadenza b)       = equation "cadenza"        (pretty b)
  pretty (LetKey key)         = equation "key"            (pretty key)
  pretty (LetMeter meter)     = equation "time"           (pretty meter)
  pretty (LetMeterPattern mp) = equation "meter_pattern"  (ppMeterPattern mp)
  pretty (LetPartial d)       = equation "partial"        (ppDuration d)
  pretty (LetRelativePitch p) = equation "relative"       (pretty p)
  pretty (LetNone)            = text "~no-bind" 

equation s e = ppcmd s <+> equals <+> e      
ppcmd = text . ('\\':) 



 
foldrMapE op f = foldr (op `onl` f)

