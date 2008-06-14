{-# LANGUAGE MultiParamTypeClasses #-}

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

import Bala.Format.Base.SymBase
import Bala.Format.SymLilyPond.Datatypes

import Text.PrettyPrint.Leijen
import Data.Char
import Data.Ratio




ppcommand :: String -> Doc
ppcommand =  text . ('\\' :)



instance SymConcatenation Ctx_Prologue P where
  (+++) l r    = P $ (unP l) <$> (unP r)

instance SymConcatenation Ctx_Note P where
  (+++) l r    = P $ (unP l) <+> (unP r)  


instance SymConcatenation Ctx_NoteAttr P where
  (+++) l r    = P $ group $ (unP l) <> (unP r) 

instance SymConcatenation Ctx_Element P where
  (+++) l r    = P $ (unP l) <+> (unP r) 
  
  
 
  
instance Attr P where
  attr a e  = P $ group $ unP e <> unP a 
  
  
instance SymCmdZero P where
  cmdZero s = P $ ppcommand s   


  
  
instance SymCmdOne P where
  cmdOne s a = P $ ppcommand s <+> unP a 


instance SymEquation P where
  equation s a = P $ text s <+> equals <+> unP a

  
instance SymDoubleQuotes P where
  doubleQuotes s = P $ dquotes $ text s 
       
  
-- comments and versioning (2.12)  
instance SymLineComment P where
  lineComment s = P $ char '%' <+> text s <> line

instance SymBlockComment P where
  blockComment s = P $ enclose (text "%{ ") (text " %}") (text s) 


 
  
  
-- pitches (6.1)
instance SymPitch P where
  pitch a       = P $ (text . map toLower . show) a   

instance SymAttrOctaveSpec P where
  raised i a     = P $ group $ unP a <> string (replicate i '\'')
  lowered i a    = P $ group $ unP a <> string (replicate i ',')


    
instance SymNote P where
  note p = P $ unP p 
  
      
-- accidentals (6.1.2)  
instance SymAttrAccidental P where
  sharp e        = P $ group $ unP e <> string "is"
  flat e         = P $ group $ unP e <> string "es"
  doubleSharp e  = P $ group $ unP e <> string "isis"
  doubleFlat e   = P $ group $ unP e <> string "eses"

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
  rest          = P $ char 'r'

-- skips (6.1.10)
instance SymSkip P where
  skip          = P $ char 's' 
  
  
-- durations (6.2)
instance SymDuration P where
  dur i e = P $ group $ unP e <> int i
  dot e   = P $ group $ unP e <> char '.'

-- tuplets (6.2.3)
instance SymTimes P where
  times n d e = P $ ppcommand "times" <+> ppfraction n d 
                      <+> braces (unP e)
  
-- chords (6.3.1)  
instance SymChord P where
  chord xs = P $ (angles $ hsep $ map unP xs)

  


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
  cmdTime r = P $ ppcommand "time" <+> pprational r

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
  vabove   = P $ char '^'
  vbelow   = P $ char '_'
  vdefault = P $ char '-'

-- accent, marcato etc. are all nullary commands so handled generically  
      
-- fingering instructions (6.6.2)
instance SymAttrFingering P where
  fingering i e     = P $ group $ unP e <> char '-' <> int i 
  
-- dynamics (6.6.3)
instance SymDynamicMark P where
  closeDynamic      = P $ ppcommand "!"
  openCrescendo     = P $ ppcommand "<"
  openDecrescendo   = P $ ppcommand ">"
  
    
-- titles and headers (10.2)

instance SymHeaderBlock P where
  headerBlock xs = P $ lbrace <$> indent 2 ((vsep $ map unP xs) <$> rbrace)
  
  