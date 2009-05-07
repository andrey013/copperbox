{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.ScorePrint
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Pretty print the internal score representation
--
--------------------------------------------------------------------------------

module Mullein.ScorePrint where 

import Mullein.CoreTypes
import Mullein.Duration
import Mullein.Pitch

import Data.Ratio
import Text.PrettyPrint.Leijen

-- TODO bar numbering

part :: Pretty e => PartP e -> Doc
part (Part es)            = lsep $ map phrase es

phrase :: Pretty e => PhraseP e -> Doc
phrase (Phrase e)         = motif e
phrase (Repeated e)       = text "_Repeat_____________" <$> motif e
phrase (FSRepeat e x y)   = text "_Repeat_Body________" <$> motif e
                        <$> text "_First_End__________" <$> motif x
                        <$> text "_Second_End_________" <$> motif y                         
 

motif :: Pretty e => MotifP e -> Doc
motif (Motif k m es)   = topline <$> (lsep $ map bar es) where
    topline = text "___" <+> keyName k <+> meterName m <+> text "___"


bar :: Pretty e => BarP e -> Doc
bar (Bar e)             = unison e
bar (Overlay e es)      = unison e <$> indent 2 (vsep $ map fn es) 
                          where fn = (text "|+" <+>) .  unison  

unison :: Pretty e => UnisonP e -> Doc
unison (Unison es t)    = (foldr (</>) empty $ map bracket es) <+> tied t

tied :: Bool -> Doc
tied True               = text "~~~"
tied False              = empty

bracket :: Pretty e => BracketP e -> Doc
bracket (Singleton e)   = element e
bracket (Bracket es)    = tildesep $ map element es

element :: Pretty e => ElementP e -> Doc
element (Note e d)      = pretty e `djoin` durationName d
element (Rest d)        = char 'r' `djoin` durationName d
element (Spacer d)      = char 'z' `djoin` durationName d                 
element (Chord es d)    = brackets (hsep $ map pretty es) `djoin` durationName d
element (GraceNotes es) = braces (hsep $ map fn es) where
                          fn (e,d) = pretty e `djoin` durationName d



keyName :: Key -> Doc
keyName (Key (PitchLabel l a) m) = pitch1 l a <+> modeName m

modeName :: Mode -> Doc
modeName Major          = text "maj"
modeName Minor          = text "minor"
modeName Lydian         = text "lydian"
modeName Ionian         = text "ionian"
modeName Mixolydian     = text "mixolydian"
modeName Dorian         = text "dorian"
modeName Aeolian        = text "aeolian"
modeName Phrygian       = text "phrygian"   
modeName Locrian        = text "locrian"

meterName :: Meter -> Doc
meterName (TimeSig n d) = integer n <> char '/' <> integer d
meterName CommonTime    = text "4/4"
meterName CutTime       = text "2/4"

pitchName :: Pitch -> Doc
pitchName (Pitch l a o) = pitch1 l a <> prime <> int o 

pitch1 :: PitchLetter -> Accidental -> Doc
pitch1 l a = char (toLowerLChar l) <> accidentalName a

accidentalName :: Accidental -> Doc
accidentalName DoubleFlat   = text "ff"
accidentalName Flat         = text "f"
accidentalName Nat          = empty
accidentalName Sharp        = text "s"
accidentalName DoubleSharp  = text "ss"

durationName :: Duration -> Doc
durationName d
    | d == 4        = text "longa"
    | d == 2        = text "breve"
    | d == 3%2      = text "dwn"
    | d == 1        = text "wn"
    | d == 3%4      = text "dhn"
    | d == 1%2      = text "hn"
    | d == 3%8      = text "dqn"
    | d == 1%4      = text "qn"    
    | d == 3%16     = text "den"
    | d == 1%8      = text "en"
    | d == 3%32     = text "dsn"
    | d == 1%16     = text "sn"
    | d == 3%64     = text "dtn"
    | d == 1%32     = text "tn"
    | otherwise     = text $ show d



--------------------------------------------------------------------------------
-- helpers

prime :: Doc
prime = char '\''

djoin :: Doc -> Doc -> Doc
djoin a b = a <> char '.' <> b

tildesep :: [Doc] -> Doc
tildesep = foldr tilde empty where 
    tilde a b = a <> char '~' <> b

lsep :: [Doc] -> Doc
lsep = foldr f empty where f a b = a <$> empty <$> b