{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

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


module HNotate.PPInstances where

import HNotate.CommonUtils
import HNotate.Document
import HNotate.Duration
import HNotate.Env
import HNotate.MusicRepDatatypes
import HNotate.NotateMonad
import HNotate.NoteListDatatypes
import HNotate.OnsetQueue
import HNotate.Pitch
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
      eline s f pp = fillString 20 s <> colon <+> pp (f e)  
      
      ppfun        = const $ text "<fun>"
      
      optAnacrusis (Just d)   = ppDuration d   
      optAnacrusis Nothing    = text "none"
      
      optPitch (Just p)       = pp p   
      optPitch Nothing        = text "none"             

instance Witness Env where textrep = wpp . pp

--------------------------------------------------------------------------------
-- NoteListDatatypes

instance PP Tile where
  pp (Singleton e)       = pp e
  
  pp (Chord se d a)      = brackets (hsep $ unseqMap pp se) <> prime <> pp d
      
  pp (GraceNotes se m a) = braces (hsep $ unseqMap fn se) 
    where fn (p,d) = pp p <> prime <> pp d
  
instance PP Glyph where
  pp (Note p d a)         = applyLyAnno a (pp p <> prime <> pp d)
  
  pp (Rest Marked d a)    = applyLyAnno a (char 'r' <> pp d)
  
  pp (Rest Spacer d a)    = applyLyAnno a (char 's' <> pp d)
  
  
  pp (RhythmicMark l d m) = text l <> prime <> pp d
      
  pp (Mark l m)           = text l
  
  

instance PP OutputFormat where
  pp Abc        = text "Abc"
  pp Ly         = text "LilyPond"  

instance (PP e) => PP (NoteListF e) where
  pp (NoteList se)        = genPunctuateSeq pp line se

instance (PP e) => Witness (NoteListF e) where
  textrep = wpp . pp

instance (PP e) => PP (BlockF e) where
  pp (SingleBlock i e)    = measureNumber i <&\> indent 4 (pp e)
  pp (PolyBlock i se)     = 
      measureNumber i <&\> indent 4 (dblangles' $ vsep $ punctuate 
                                               (text " // ")
                                               (map pp $ F.toList se))

measureNumber :: Int -> ODoc
measureNumber i = text "|:" <>  int i


instance (PP e) => PP (BarF e) where
  pp (Bar se)             = genPunctuateSeq pp space se




durationSuffix :: Duration -> ODoc
durationSuffix d = char '\'' <> ppDuration d 



--------------------------------------------------------------------------------
-- MusicRepDatatypes
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



instance PP a => PP (OnsetQueue a) where
  pp = foldlOnsetQueue fn emptyDoc
    where fn d (i,xs) = d <&\> int i <+> text ":+" <+> list (map pp xs)    


instance PP a => Witness (OnsetQueue a) where
  textrep = wpp . pp

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
  pp (Let bind e) = text "let"  <+> pp bind <+> text "in"
                                <&\> indent 2 (pp e)   
  pp (SDo out e)  = text "sdo"  <+> pp out <+> text "then" <+> pp e   
  pp (Do out)     = text "do"   <+> pp out <+> text "end"  
  pp (Fork e1 e2) = text "fork" <+> 
                              nest 2 (text "<<" <+>  pp e1 
                                                <&\> text "//" 
                                                <+>  pp e2)
                             <&\> text ">>"
                                      
instance Witness [Expr] where
  textrep = wpp . vsep . map pp
        
instance PP OutputDirective where
  pp (OutputDirective oscm name) = text "#output" <> fn oscm <+> text name
    where fn Nothing  = emptyDoc
          fn (Just x) = char ' ' <> pp x  
     

instance PP Binding where
  pp (LetCadenza b)       = equation "cadenza"        (pp b)
  pp (LetKey key)         = equation "key"            (pp key)
  pp (LetMeter meter)     = equation "time"           (pp meter)
  pp (LetMeterPattern mp) = equation "meter_pattern"  (ppMeterPattern mp)
  pp (LetPartial d)       = equation "partial"        (ppDuration d)
  pp (LetRelativePitch p) = equation "relative"       (pp p)
  pp (LetNone)            = text "~no-bind" 

equation s e = ppcmd s <+> equals <+> e      
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
genPunctuateSeq pp sep = para phi emptyDoc
  where 
    phi c (se,  d)  | null se        = pp c <+> d 
                    | otherwise      = pp c <> sep <+> d
                    
                    
 
foldrMapE op f = foldr (op `onl` f)

