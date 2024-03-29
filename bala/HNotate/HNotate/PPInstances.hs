{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}
{-# OPTIONS -fno-warn-orphans #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.PPInstances
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined
--
-- Debugging helpers - PP and Witness instances for 
-- internal datatypes.
--
--------------------------------------------------------------------------------


-- Don't warn on orphaned instances in this only. 

module HNotate.PPInstances where

import HNotate.CommonUtils
import HNotate.Document
import HNotate.Duration
import HNotate.Env
import HNotate.MusicRepDatatypes
import HNotate.NotateMonad
import HNotate.NoteListDatatypes
import HNotate.Pitch
import HNotate.ProcessingBase
import HNotate.TemplateDatatypes



import Data.Char (toLower)
import qualified Data.Foldable as F
import qualified Data.Map as Map
import Data.Ratio
import Data.Sequence
import Prelude hiding (null)
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

    

instance (Integral a, PP a) => PP (Ratio a) where
  pp r = pp (numerator r) <> char '%' <> pp (denominator r)

--------------------------------------------------------------------------------
-- Duration 

ppDuration :: Duration -> ODoc
ppDuration = pp . printableDuration

instance PP PrintableDuration where
  pp (PrintableDuration r dc) = let (n,d) = ratioElements r in 
      pp n <> char '/' <> pp d <> text (replicate dc '.') 

ppAltRest :: Char -> Duration -> ODoc
ppAltRest ch dur = char ch <> char '/' <> pp dur



--------------------------------------------------------------------------------
-- Pitch


instance PP Pitch where
  pp (Pitch l a o)  = pp l <> pp a <> int o

instance PP PitchLetter where
  pp              = text . show

instance PP Accidental where
  pp Nat          = emptyDoc
  pp Sharp        = char '#'
  pp Flat         = char 'b'
  pp DoubleSharp  = text "##"
  pp DoubleFlat   = text "bb"

ppNote :: Pitch -> Duration -> ODoc
ppNote pch dur = pp pch <> char '/' <> pp dur


--------------------------------------------------------------------------------
-- Env




instance PP Env where
  pp e  = text "Env:" <&\> 
      eline "output_format"     output_format     pp              <&\> 
      eline "current_key"       current_key       pp              <&\>
      eline "label_set"         label_set         pp              <&\> 
      eline "current_meter"     current_meter     pp              <&\> 
      eline "meter_pattern"     meter_pattern     ppMeterPattern  <&\>
      eline "bar_length"        bar_length        ppDuration      <&\>
      eline "unit_note_length"  unit_note_length  ppDuration      <&\>
      eline "relative_pitch"    relative_pitch    optPitch        <&\>
      eline "anacrusis"         anacrusis         optAnacrusis    <&\>
      eline "unmetered"         unmetered         pp              <&\>
      eline "bar_number_check"  bar_number_check  pp              <&\>
      eline "score_comment"     score_comment     ppfun          
    where 
      eline :: String -> (Env -> a) -> (a -> ODoc) -> ODoc
      eline s f pretty = fillString 20 s <> colon <+> pretty (f e)  
      
      ppfun        = const $ text "<fun>"
      
      optAnacrusis (Just d)   = ppDuration d   
      optAnacrusis Nothing    = text "none"
      
      optPitch (Just p)       = pp p   
      optPitch Nothing        = text "none"             

instance Witness Env where textrep = wpp . pp


--------------------------------------------------------------------------------
-- NoteListDatatypes

instance PP Element where
  pp (Atom e)             = pp e
  
  pp (Chord se d _)       = brackets (hsep $ fmap (pp . fst) se) 
                                <> prime <> pp d
      
  pp (GraceNotes se _)    = braces (hsep $ fmap fn se) where 
                                fn (p,d,_) = pp p <> prime <> pp d
    
  pp (Nplet _ _ se _)     = braces (hsep $ fmap (pp . fst) se)    

instance PP Atom where
  pp (Note p d _)           = pp p <> prime <> pp d
  pp (Rest d _)             = char 'r' <> pp d 
  pp (Spacer d _)           = char 's' <> pp d
  pp (RhythmicMark l d _)   = text l <> prime <> pp d
  pp (Mark l _)             = text l
  pp Tie                    = char '~'
  

instance PP OutputFormat where
  pp OutputAbc  = text "Abc"
  pp OutputLy   = text "LilyPond"


instance (PP e) => PP (NoteListF e) where
  pp (NoteList se)  = genPunctuateSeq pplBlock line (number 1 se) where
      pplBlock (i,blk) = measureNumber i <&\> pp blk
      
      
instance (PP e) => Witness (NoteListF e) where
  textrep = wpp . pp

instance (PP e) => PP (BlockF e) where
  pp (SingleBlock e)    = indent 4 (pp e)
  pp (OverlayBlock se)  = indent 4 (dblangles' $ vsep $ punctuate 
                                               (text " // ")
                                               (map pp $ F.toList se))

measureNumber :: Int -> ODoc
measureNumber i = text "|:" <>  int i


instance (PP e) => PP (BarF e) where
  pp (Bar se)             = hsep (fmap pp se)

instance PP e => PP (MetricalWordF e) where
  pp (Singleton a)   = pp a
  pp (BeamGroup se)  = char '/' <> hsep (fmap pp se) <> char '/' 


durationSuffix :: Duration -> ODoc
durationSuffix d = char '\'' <> ppDuration d 



--------------------------------------------------------------------------------
-- MusicRepDatatypes

ppMeterPattern :: MeterPattern -> ODoc
ppMeterPattern (ns,d) = 
    hcat (punctuate (char '+') $ map  pp ns) <> char '/'<> ppDuration d

instance PP Meter where
  pp (TimeSig n d)        = int n <> char '/' <> int d
  pp CommonTime           = char 'C'
  pp CutTime              = text "C|"


instance PP PitchLabel where
  pp (PitchLabel l a)     = pp l <> pp a

instance PP Key where
  pp (Key pl mode accs)   = pp pl <+> char '\\' <> pp mode <> ppAccs accs
    where
      ppAccs [] = emptyDoc
      ppAccs xs = hsep $ map pp xs
  
instance PP Mode where
  pp = text . fmap toLower . show
  
  
instance PP LabelSet where
  pp (LabelSet mp) = 
      hcat $ punctuate (char '-') (map (pp . snd) $ Map.toAscList mp) 




--------------------------------------------------------------------------------
-- TemplateDatatypes

ppPos :: SrcLoc -> ODoc 
ppPos (SrcLoc l c) = tupled $ [pp l, pp c]

  
instance PP OutputScheme where
  pp OutputRelative       = text "relative" 
  pp OutputDefault        = text "default"


-- shared with LilyPond
instance PP MetaOutput where
  pp (MetaOutput oscm name) = bananas body
    where body      = text "#output" <> colon <+> optscheme <+> text name
          optscheme = maybe emptyDoc pp oscm


-- shared with LilyPond  
instance PP MetaBinding where
  pp (MetaMeterPattern mp)  = metabind "meter_pattern" (ppMeterPattern mp)
  pp (MetaPartial d)        = metabind "partial" (pp d)

metabind :: String -> ODoc -> ODoc  
metabind name d = bananas $ text ('~':name) <> colon <+> d  
  
instance PP AbcScore where
  pp (AbcScore xs) = vsep $ map pp xs

instance Witness AbcScore where textrep = wpp . pp


-- X field gives the Int
instance PP AbcTune where
  pp (AbcTune n xs)         = 
      text "X:" <+> int n <+> indent 2 (list $ map pp xs)

  
instance PP AbcExpr where
  pp (AbcFieldBinding field)      = pp field
  pp (AbcMetaBinding mb)          = pp mb
  pp (AbcOutput mo)               = pp mo
             
             
instance PP AbcField where
  pp (AbcKey Nothing)   = text "K:"
  pp (AbcKey (Just k))  = text "K:" <> pp k
  pp (AbcMeter m)       = text "M:" <> pp m

  
    
instance PP LyScore where
  pp (LyScore xs) = vsep $ map pp xs
 
instance Witness LyScore where textrep = wpp . pp

  
instance PP LyExpr where
  pp (LyCmdBinding cmd)   = pp cmd
  pp (LyMetaBinding mb)   = pp mb 
  pp (LyOutput mo)        = pp mo
  pp (LyNestExpr es)      = indent 2 $ list (map pp es)
  


instance PP LyCommand where
  pp (LyCadenza True)   = text "\\cadenzaOn"
  pp (LyCadenza False)  = text "\\cadenzaOn"
  pp (LyKey key)        = text "\\key"        <+> pp key
  pp (LyPartial d)      = text "\\partial"    <+> pp d
  pp (LyRelative p)     = text "\\relative"   <+> pp p
  pp (LyTime meter)     = text "\\time"       <+> pp meter
  
    
  
instance PP Expr where
  pp (Let bind expr)      = text "let"    <+>  pp bind <+> text "in"
                                              <&\> indent 2 (pp expr)   
  pp (DoExpr out expr)    = text "doexpr" <+> pp out <+> text "then" <+> pp expr   
  pp (Do out)             = text "do"     <+> pp out <+> text "end"  
  pp (Fork expr1 expr2)   = text "fork"   <+> 
                              indent 2 (text "<<" <+>  pp expr1 
                                                  <&\> text "//" 
                                                  <+>  pp expr2)
                             <&\> text ">>"
                                      
instance Witness [Expr] where
  textrep = wpp . vsep . map pp
        
instance PP OutputDirective where
  pp (OutputDirective oscm name) = text "#output " <> fn oscm <+> text name
    where fn Nothing  = emptyDoc
          fn (Just x) = pp x  
     

instance PP Binding where
  pp (LetCadenza b)       = equation "cadenza"        (pp b)
  pp (LetKey key)         = equation "key"            (pp key)
  pp (LetMeter meter)     = equation "time"           (pp meter)
  pp (LetMeterPattern mp) = equation "meter_pattern"  (ppMeterPattern mp)
  pp (LetPartial d)       = equation "partial"        (ppDuration d)
  pp (LetRelativePitch p) = equation "relative"       (pp p)
  pp (LetNone)            = text "~no-bind" 

equation :: [Char] -> ODoc -> ODoc
equation s e = ppcmd s <+> equals <+> e

ppcmd :: [Char] -> ODoc     
ppcmd = text . ('\\':) 




-- finger -- a pretty printer for seqeunces 
-- c.f. tupled or list in PPrint - type of the param is different: 
-- Pretty a => Seq a, rather than Seq Doc
finger :: PP a => Seq a -> ODoc
finger = enclose (text "(|") (text "|)") . genPunctuateSeq pp comma


genFinger :: (a -> ODoc) -> Seq a -> ODoc
genFinger f = enclose (text "(|") (text "|)") . genPunctuateSeq f comma

-- para gives us a 'view of the remaining sequence' along with the current 
-- element - so we can get a proper intersperse.
-- A fold can only see the current element so it would do add an extra sep
-- e.g.: 1,2,3, 
genPunctuateSeq :: (a -> ODoc) -> ODoc -> Seq a -> ODoc
genPunctuateSeq fn sep = para phi emptyDoc
  where 
    phi c (se,  d)  | null se        = fn c <+> d 
                    | otherwise      = fn c <> sep <+> d
                    
                    
-- foldrMapE op f = foldr (op `onl` f)

