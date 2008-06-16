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



cmd :: String -> Doc
cmd =  text . ('\\' :)


bracesSpaced :: Doc -> Doc
bracesSpaced = enclose (lbrace <> space) (space <> rbrace)

bracesHanging :: Doc -> Doc
bracesHanging d = lbrace <$> indent 2 (d <$> rbrace)

ppcommand :: String -> Doc
ppcommand =  text . ('\\' :)



instance SymConcatenation Ctx_Top P where
  (+++) l r    = P $ (unP l) <$> (unP r)



instance SymConcatenation Ctx_Book P where
  (+++) l r    = P $ (unP l) <$> (unP r)
  
instance SymConcatenation Ctx_Header P where
  (+++) l r    = P $ (unP l) <$> (unP r)

  
instance SymConcatenation Ctx_Note P where
  (+++) l r    = P $ (unP l) <+> (unP r)  


instance SymConcatenation Ctx_NoteAttr P where
  (+++) l r    = P $ group $ (unP l) <> (unP r) 

instance SymConcatenation Ctx_Element P where
  (+++) l r    = P $ (unP l) <+> (unP r) 
  

  
    
instance Attr P where
  attr a e  = P $ group $ unP e <> unP a 
        
  
-- comments and versioning (2.12)  

instance SymCmdVersion P where 
  version s       = P $ cmd "version" <+> dquotes (text s)
  
  
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
  
  
-- Micro tones (6.1.4)    
instance SymAttrMicroTone P where
  halfFlat a      = P $ group $ unP a <> string "ih" 
  halfSharp a     = P $ group $ unP a <> string "es"

-- Relative octaves (6.1.6)

instance SymCmdReleative P where
  relative p e  = P $ cmd "relative" <+> unP p <+> bracesHanging (unP e) 

  
-- rests (6.1.9)
instance SymRest P where
  rest          = P $ char 'r'

-- skips (6.1.10)
instance SymCmdSkip P where
  cmdSkip s        = P $ text s
  
instance SymSkipDuration P where
  skipDuration d    = P $ group $ char 's' <> unP d
  
  
-- durations (6.2)

instance SymDuration P where
  duration i = P $ int i
  
  
instance SymAttrDuration P where
  attrduration d e = P $ group $ unP e <> unP d



instance SymAttrDotted P where
  dotted i a   = P $ group $ unP a <> text (replicate i '.') 
  
instance SymAttrCmdLongDuration P where
  cmdLongDuration s a = P $ group $ unP a <> cmd s
  
-- tuplets (6.2.3)
instance SymTimes P where
  times r e = P $ ppcommand "times" <+> pretty r <+> braces (unP e)
  
-- chords (6.3.1)  
instance SymChord P where
  chord xs = P $ (angles $ hsep $ map unP xs)

  


-- stems (6.3.2)
instance SymCmdStem P where
  cmdStem s   = P $ cmd s

-- polyphony (6.3.3)

instance SymPolyCat P where
  a \\ b = P $ unP a <+> text "\\\\" <$> unP b


-- clef (6.4.1)
instance SymCmdClef P where
  clef ct       = P $ cmd "clef" <+> unP ct
  
instance SymClefType P where
  cleftype s = P $ text s

transpClef :: Doc -> Int -> Doc
transpClef d i   
    | i > 0           = dquotes $ group $ d <> char '^' <> int i
    | i == 0          = d
    | otherwise       = dquotes $ group $ d <> char '_' <> int (abs i)

instance SymAttrClefTransposition P where  
  clefTransposition n a      = P $ transpClef (unP a) n




-- Key signature (6.4.2)

instance SymCmdKey P where
  key a b         = P $ cmd "key" <+> unP a <+> unP b

instance SymCmdKeyType P where
  keyType s       = P $ cmd s
  

-- Time signature (6.4.3) 
instance SymCmdTime P where
  time r = P $ cmd "time" <+> pretty r

-- Bar lines (6.4.5)
instance SymCmdBar P where
  bar s    = P $ cmd "bar" <+> text s 

-- Unmetered music (6.4.6)

instance SymCmdCadenza P where
  cmdCadenza s     = P $ cmd s

    

-- Ties (6.5.1)
instance SymTie P where
  tie         = P $ char '~'
  
instance SymCmdTie P where
  cmdTie s      = P $ cmd s  
  
  
-- Slurs (6.5.2)
instance SymSlur P where
  openSlur    = P $ char '('
  closeSlur   = P $ char ')'


instance SymCmdSlur P where
  cmdSlur s      = P $ cmd s  
  
    
  
-- Phrasing slurs (6.5.3)
instance SymCmdPhrasingSlur P where
  cmdPhrasingSlur s      = P $ cmd s


-- Laissez vibrer ties (6.5.4)
instance SymAttrCmdLaissezVibrer P where
  laissezVibrer e   = P $ group $ unP e <> cmd "laissezVibrer"
  
  
-- Automatic beams (6.5.5)  
instance SymAttrCmdNoBeam P where
  noBeam e    = P $ group $ unP e <> cmd "noBeam"

-- Manual beams (6.5.6)  
instance SymBeam P where
  openBeam  = P $ char '['
  closeBeam = P $ char ']'

-- Grace notes (6.5.7)

instance SymCmdGrace P where
  cmdGrace s            = P $ cmd s

  

-- Articulations (6.6.1) 

instance SymCmdArticulation P where
  cmdArticulation s    = P $ cmd s
  
  
instance SymVerticalPlacement P where
  vabove   = P $ char '^'
  vbelow   = P $ char '_'
  vdefault = P $ char '-'
 
      
-- Fingering instructions (6.6.2)
instance SymAttrFingering P where
  fingering i e     = P $ group $ unP e <> char '-' <> int i 
  
-- Dynamics (6.6.3)

instance SymCmdDynamic P where
  cmdDynamic s         = P $ cmd s
  


-- Breath marks (6.6.4)

instance SymCmdBreathe P where
  cmdBreathe s         = P $ cmd s
  
-- glissando (6.6.6)

instance SymCmdGlissando P where
  cmdGlissando s       = P $ cmd s
  
-- arpeggio (6.6.7)

instance SymCmdArpeggio P where
  cmdArpeggio s        = P $ cmd s

  
-- falls and doits (6.6.8)

instance SymCmdBendAfter P where
  bendAfter         = P $ cmd "bendAfter"
  
    
-- Metronome marks (8.2.2)

instance SymCmdTempo P where
  tempo a i         = P $ cmd "tempo" <+> group (unP a <> equals <> int i)


-- Creating contexts (9.2.2)

instance SymCmdNew P where
  newContext ct e  = P $ cmd "new" <+> unP ct <+> braces (unP e)   
  
instance SymContextType P where
  contextType s     = P $ text s


-- Multiple scores in a book (10.1.2)

instance SymCmdScore P where
  score e     = P $ cmd "score" <+> bracesHanging (unP e)

instance SymCmdMarkup P where
  markup s    = P $ cmd "markup" <+> bracesSpaced (text s)


instance SymCmdBook P where
  book e      = P $ cmd "book" <+> bracesHanging (unP e)
  
      
-- titles and headers (10.2)

instance SymCmdHeader P where
  header a       = P $ cmd "header" <+> bracesHanging (unP a) 
        
  
    
instance SymBlock P where
  block e = P $ braces $ unP e

equation :: String -> Doc -> Doc
equation var d = text var <+> equals <+> d

instance SymEqnTitle P where
  title s = P $ equation "title" (dquotes $ text s)
  
instance SymEqnDedication P where
  dedication s = P $ equation "dedication" (dquotes $ text s)
    