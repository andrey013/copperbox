--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Format.SymLilyPond.Pretty
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Pretty printer for a subset of LilyPond files
--
--------------------------------------------------------------------------------

module Bala.Format.SymLilyPond.Pretty where

import Bala.Format.SymLilyPond.Datatypes

import Text.PrettyPrint.Leijen
import Data.Char


newtype P a = P { unP :: Doc }

putDocP x = putDoc $ unP (x ())


ppcommand :: String -> Doc
ppcommand =  text . ('\\' :)

ppfraction :: Int -> Int -> Doc
ppfraction n d = group $ int n <> char '/' <> int d 


instance SymExpr P where
  emp         = P $ empty
  (##) l r    = P $ (unP l) <+> (unP r)

instance SymMaybe P where
  just a  = P $ unP a
  nothing = P $ empty  

  

instance SymCmdZero P where
  cmdZero s = P $ ppcommand s    
    
-- pitches (6.1)
instance SymPitchName P where
  pitchName a = P $ (text . map toLower . show) a   

instance SymOctaveSpec P where
  raised i      = P $ string (replicate i '\'')
  lowered i     = P $ string (replicate i ',')

instance SymPitch P where
  pitch n oa oo = P $ group $ unP n <> unP oa <> unP oo  

    
instance SymNote P where
  note p d = P $  group $ unP p <> unP d 
  
      
-- accidentals (6.1.2)  
instance SymAccidental P where
  sharp         = P $ string "is"
  flat          = P $ string "es"
  doubleSharp   = P $ string "isis"
  doubleFlat    = P $ string "eses"

-- cautionary accidentals (6.1.3)
instance SymCautionaryAccidental P where
  reminderAccidental    = P $ char '!'
  cautionaryAccidental  = P $ char '?'
  
  
-- micro tones (6.1.4)    
instance SymMicroTone P where
  halfFlat      = P $ string "ih" 
  halfSharp     = P $ string "es"


  
-- rests (6.1.9)
instance SymRest P where
  rest a = P $ group $ char 'r' <> unP a

-- durations (6.2)
instance SymDuration P where
  dur i = P $ int i
  dot i = P $ group $ int i <> char '.'

-- tuplets (6.2.3)
instance SymTimes P where
  times n d e = P $ ppcommand "times" <+> ppfraction n d 
                      <+> braces (unP e)
  
-- chords (6.3.1)  
instance SymChord P where
  chord xs d = P $ (angles $ hsep $ map unP xs) <> unP d

  


-- stems (6.3.2)
-- stemUp etc are all nullary commands so handled generically

-- polyphony (6.3.3)

instance SymPolyCat P where
  a \\ b = P $ unP a <+> text "\\\\" <$> unP b


-- clef (6.4.1)
instance SymCmdClef P where
  cmdClef a = P $ ppcommand "clef" <+> unP a
  

instance SymClef P where
  clef s = P $ text s

-- key signature (6.4.2)
-- major, minor, ionian etc. are all nullary commands so handled generically 

-- time signature (6.4.3)  
instance SymCmdTime P where
  cmdTime n d = P $ ppcommand "time" <+> ppfraction n d

-- barlines (6.4.5)
instance SymCmdBar P where
  cmdBar s    = P $ ppcommand "bar" <+> text s 

-- unmetered music (6.4.6)
-- cadenzaOn, cadenzaOff are nullary commands so handled generically 
    

-- ties (6.5.1)
instance SymTie P where
  tie         = P $ char '~'
  

-- slurs (6.5.2)
instance SymSlur P where
  openSlur    = P $ char '('
  closeSlur   = P $ char ')'
  
  
-- phrasing slurs (6.5.3)
instance SymPhrasingSlur P where
  openPhrasingSlur    = P $ ppcommand "("
  closePhrasingSlur   = P $ ppcommand ")"
  
  

-- beams (6.5.6)  
instance SymBeam P where
  openBeam  = P $ char '['
  closeBeam = P $ char ']'
  

-- articulations (6.6.1) 

instance SymVerticalPlacement P where
  vAbove   = P $ char '^'
  vBelow   = P $ char '_'
  vDefault = P $ char '-'

-- accent, marcato etc. are all nullary commands so handled generically  
      
-- fingering instructions (6.6.2)
instance SymFingeringInst P where
  fingeringInst i   = P $ group (char '-' <> int i) 
  
-- dynamics (6.6.3)
instance SymDynamicMark P where
  closeDynamic      = P $ ppcommand "!"
  openCrescendo     = P $ ppcommand "<"
  openDecrescendo   = P $ ppcommand ">"
  
    
  