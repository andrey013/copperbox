{-# LANGUAGE MultiParamTypeClasses, PatternSignatures #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Format.SymLilyPond.Pretty
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  multi-parameter typeclasses
--
-- Pretty printer for a subset of LilyPond files
--
--------------------------------------------------------------------------------

module Bala.Format.SymLilyPond.Pretty where

import Bala.Format.Base.SymBase
import Bala.Format.SymLilyPond.Datatypes

import Text.PrettyPrint.Leijen
import Data.Char


-- | Prefix a command name with \\.
cmd :: String -> Doc
cmd =  text . ('\\' :)

-- | Version of pprint's braces that inserts spaces between the braces 
-- e.g. @{ content }@. LilyPond can be sensitive to this. 
bracesSpaced :: Doc -> Doc
bracesSpaced = enclose (lbrace <> space) (space <> rbrace)

-- | Version of pprint's braces that hangs its content: 
-- 
-- > ... {
-- >   content
-- > }
bracesHanging :: Doc -> Doc
bracesHanging d = lbrace <$> indent 2 (d <$> rbrace)



instance CSnocList P CT_Toplevel where
  snil                = P $ empty
  snoc xs x           = P $ unP xs <$> unP x


instance CSnocList P CT_Element where
  snil                = P $ empty
  snoc xs x           = P $ unP xs <+> unP x

instance CSnocList P CT_Header where
  snil                = P $ empty
  snoc xs x           = P $ unP xs <$> unP x
  
instance CSnocList P CT_Book where
  snil                = P $ empty
  snoc xs x           = P $ unP xs <$> unP x

  
    
-- instance CAttr P where
--   attr e a              = P $ group $ unP e <> unP a 

-- instance CPrefixAttr P where
--  prefixAttr a e        = P $ group $ unP a <> unP e 
  
  
        
--------------------------------------------------------------------------------
-- *** Commenting input files (2.12)  

instance CCmdVersion P where 
  version s             = P $ cmd "version" <+> dquotes (text s)
  
  
instance CLineComment P where
  lineComment s         = P $ char '%' <+> text s <> line

instance CBlockComment P where
  blockComment s        = P $ enclose (text "%{ ") (text " %}") (text s) 


 
  
--------------------------------------------------------------------------------
-- * Basic notation (6)
-- ** Pitches (6.1)
-- *** Normal pitches (6.1.1) (6.1)
instance CPitch P where
  pitch a               = P $ (text . map toLower . show) a   

instance COctaveSpec P where
  raised i              = P $ string (replicate i '\'')
  lowered i             = P $ string (replicate i ',')


    
instance CNote P where
  note p                = P $ unP p 
  
--------------------------------------------------------------------------------      
-- *** Accidentals (6.1.2)  
instance CAccidental P where
  sharp                 = P $ string "is"
  flat                  = P $ string "es"
  doubleSharp           = P $ string "isis"
  doubleFlat            = P $ string "eses"

--------------------------------------------------------------------------------
-- *** Cautionary accidentals (6.1.3)
instance CCautionaryAccidental P where
  reminderAccidental    = P $ char '!'
  cautionaryAccidental  = P $ char '?'
  
--------------------------------------------------------------------------------
-- *** Micro tones (6.1.4)    
instance CMicroTone P where
  halfFlat              = P $ string "ih" 
  halfSharp             = P $ string "es"

--------------------------------------------------------------------------------
-- *** Relative octaves (6.1.6)

instance CCmdRelative P where
  relative p e          = P $ cmd "relative" <+> unP p <+> bracesHanging (unP e) 

--------------------------------------------------------------------------------  
-- *** Rests (6.1.9)
instance CRest P where
  rest                  = P $ char 'r'

--------------------------------------------------------------------------------
-- *** Skips (6.1.10)
instance CCmdSkip P where
  cmdSkip s             = P $ text s
  
instance CSkipDuration P where
  skipDuration d        = P $ group $ char 's' <> unP d
  
--------------------------------------------------------------------------------
-- ** Rhythms (6.2)
-- *** Durations (6.2)

  
instance CDuration P where
  duration i            = P $ int i

-- breve and longa could be members of SymDuration  
--  breve                 = P $ cmd "breve"
--  longa                 = P $ cmd "longa"


instance CCmdLongDuration P where
  cmdLongDuration s     = P $ cmd s



--------------------------------------------------------------------------------
-- *** Augmentation dots (6.2.2)
instance CDotted P where
  dotted i              = P $ text (replicate i '.') 
  
--------------------------------------------------------------------------------
-- *** Tuplets (6.2.3)
instance CCmdTimes P where
  cmdTimes r e          = P $ cmd "times" <+> pretty r <+> braces (unP e)

--------------------------------------------------------------------------------
-- ** Mutliple notes at once (6.3)
-- *** Chords (6.3.1)  
instance CChord P where
  chord xs              = P $ (angles $ hsep $ map unP xs)

  

--------------------------------------------------------------------------------
-- *** Stems (6.3.2)
instance CCmdStem P where
  cmdStem s             = P $ cmd s

--------------------------------------------------------------------------------
-- *** Polyphony (6.3.3)

instance CPoly P where
  openPoly              = P $ text "<< " <> line
  closePoly             = P $ line <> text " >>"
  a \\ b                = P $ unP a <+> text "\\\\" <$> unP b

--------------------------------------------------------------------------------
-- ** Staff notation (6.4)
-- *** Clef (6.4.1)
instance CCmdClef P where
  clef ct               = P $ cmd "clef" <+> unP ct <> linebreak
  
instance CClefType P where
  cleftype s            = P $ text s

-- *** Clef Transposition - TODO
{-
transpClef :: Doc -> Int -> Doc
transpClef d i   
    | i > 0           = dquotes $ group $ d <> char '^' <> int i
    | i == 0          = d
    | otherwise       = dquotes $ group $ d <> char '_' <> int (abs i)

instance CClefTransposition P where  
  clefTransposition n   = P $ transpClef (unP a) n
-}


--------------------------------------------------------------------------------
-- *** Key signature (6.4.2)

instance CCmdKey P where
  key a b               = P $ cmd "key" <+> unP a <+> unP b <> linebreak

instance CCmdKeyType P where
  keyType s             = P $ cmd s
  
--------------------------------------------------------------------------------
-- *** Time signature (6.4.3) 
instance CCmdTime P where
  time r                = P $ cmd "time" <+> pretty r

--------------------------------------------------------------------------------
-- *** Bar lines (6.4.5)
instance CCmdBar P where
  bar s                 = P $ cmd "bar" <+> text s 

--------------------------------------------------------------------------------
-- *** Unmetered music (6.4.6)

instance CCmdCadenza P where
  cmdCadenza s          = P $ cmd s

    
--------------------------------------------------------------------------------
-- ** Connecting notes (6.5)
-- *** Ties (6.5.1)
instance CTie P where
  tie                   = P $ char '~'
  
instance CCmdTie P where
  cmdTie s              = P $ cmd s  
  
--------------------------------------------------------------------------------
-- *** Slurs (6.5.2)
instance CSlur P where
  openSlur              = P $ char '('
  closeSlur             = P $ char ')'


instance CCmdSlur P where
  cmdSlur s             = P $ cmd s  
  
    
--------------------------------------------------------------------------------
-- *** Phrasing slurs (6.5.3)
instance CCmdPhrasingSlur P where
  cmdPhrasingSlur s      = P $ cmd s

--------------------------------------------------------------------------------
-- *** Laissez vibrer ties (6.5.4)
instance CCmdLaissezVibrer P where
  laissezVibrer         = P $ cmd "laissezVibrer"
  
--------------------------------------------------------------------------------
-- *** Automatic beams (6.5.5)  
instance CCmdNoBeam P where
  noBeam                = P $ cmd "noBeam"

--------------------------------------------------------------------------------
-- *** Manual beams (6.5.6)  
instance CBeam P where
  openBeam              = P $ char '['
  closeBeam             = P $ char ']'

--------------------------------------------------------------------------------
-- *** Grace notes (6.5.7)

instance CCmdGrace P where
  cmdGrace s            = P $ cmd s

  
--------------------------------------------------------------------------------
-- ** Expressive marks (6.6)
-- *** Articulations (6.6.1) 

instance CCmdArticulation P where
  cmdArticulation s     = P $ cmd s
  
instance CArticulation P where
  articulation s        = P $ text s
                   
    
instance CVerticalPlacement P where
  verticalPlacement VAbove     = P $ char '^'
  verticalPlacement VBelow     = P $ char '_'
  verticalPlacement VDefault   = P $ char '-'
  
 
--------------------------------------------------------------------------------
-- *** Fingering instructions (6.6.2)
instance CFingering P where
  fingering i           = P $ group $ char '-' <> int i 

--------------------------------------------------------------------------------
-- *** Dynamics (6.6.3)

instance CCmdDynamic P where
  cmdDynamic s          = P $ cmd s
  

--------------------------------------------------------------------------------
-- *** Breath marks (6.6.4)

instance CCmdBreathe P where
  cmdBreathe s         = P $ cmd s

--------------------------------------------------------------------------------
-- *** Glissando (6.6.6)

instance CCmdGlissando P where
  cmdGlissando s       = P $ cmd s

--------------------------------------------------------------------------------
-- *** Arpeggio (6.6.7)

instance CCmdArpeggio P where
  cmdArpeggio s        = P $ cmd s

--------------------------------------------------------------------------------
-- *** Falls and doits (6.6.8)

instance CCmdBendAfter P where
  bendAfter             = P $ cmd "bendAfter"

--------------------------------------------------------------------------------
-- * Instrument-specific notation (7)
-- ** Piano music (7.1)
-- *** Automatic staff changes (7.1.1)

instance CCmdAutochange P where
  autochange            = P $ cmd "autochange"
  
-- *** Pedals (7.1.2)
instance CCmdPedal P where
  cmdPedal s            = P $ cmd s
  
--------------------------------------------------------------------------------
-- ** Chord names (7.2)
-- *** Chords mode (7.2.2)


instance CCmdChordmode P where
  chordmode a      = P $ cmd "chordmode" <+> bracesHanging (unP a)
  
--------------------------------------------------------------------------------
-- ** Vocal music (7.3)
-- *** Setting simple songs (7.3.1)

instance CCmdAddlyrics P where
  addlyrics s       = P $ cmd "addlyrics" <+> bracesSpaced (text s)

-- *** Melismata (7.3.5)
instance CMelismata P where
  melisma           = P $ cmd "melisma"
  melismaEnd        = P $ cmd "melismaEnd"
  
  
--------------------------------------------------------------------------------
-- ** Rhythmic music (7.4)
-- *** Showing melody rhythms(7.4.1)
  
instance CCtxRhythmicStaff P where  
  rhythmicStaff       = P $ text "RhythmicStaff"
  
--------------------------------------------------------------------------------
-- *** Entering percussion (7.4.2)

instance CCmdDrums P where
  drums e             = P $ cmd "drums" <+> bracesHanging (unP e)
  
--------------------------------------------------------------------------------
-- ** Guitar (7.5)

-- *** Tablatures basic (7.5.2)

-- | stringnum corresponds to @\\@ in LilyPond.

instance CStringnum P where
  stringnum i           = P $ group $ char '\\' <> int i

instance CCtxTabStaff P where  
  tabStaff              = P $ text "TabStaff"
  
instance CCtxTabVoice P where  
  tabVoice              = P $ text "TabVoice"
  
-- *** Right hand fingerings (7.5.6)

instance CRightHandFinger P where
  rightHandFinger i     =  P $ group $ 
                               text "-\rightHandFinger" <+> char '#' <+> int i

--------------------------------------------------------------------------------
-- ** Other instrument specific notation (7.8)
-- *** Artificial harmonics (strings) (7.8.1)

instance CCmdHarmonic P where
  cmdHarmonic           = P $ cmd "harmonic"
  
--------------------------------------------------------------------------------
-- * Advanced notation (8)
-- ** Text (8.1)
-- *** Text scripts (8.1.1)

instance CTextScript P where
  textScript s       = P $ dquotes (text s)   
  
instance CCmdFatText P where
  fatText           = P $ cmd "fatText"
 
-- *** Text markup (8.1.4)

instance CCmdMarkup P where
  markup s          = P $ cmd "markup" <+> bracesSpaced (text s)
  
    
--------------------------------------------------------------------------------
-- ** Preparing parts (8.2)
-- *** Metronome marks (8.2.2)

instance CCmdTempo P where
  tempo a i         = P $ cmd "tempo" <+> group (unP a <> equals <> int i)

--------------------------------------------------------------------------------
-- * Changing defaults (9)
-- ** Interpretation contexts (9.2)
-- *** Creating contexts (9.2.2)

instance CCmdNew P where
  newContext ct e   = P $ cmd "new" <+> unP ct <+> braces (unP e)   


instance CCtxStaff P where
  staff             = P $ text "Staff"

instance CCtxVoice P where  
  voice             = P $ text "Voice"
  

  
  
{-  
instance CContextType P where
  contextType s     = P $ text s
-}

--------------------------------------------------------------------------------
-- * Non-musical notation (10)
-- ** Input files (10.1)

-- ** Titles and headers (10.2)
-- *** Multiple scores in a book (10.1.2)

instance CCmdScore P where
  score e     = P $ cmd "score" <+> unP e



instance CCmdBook P where
  book e      = P $ cmd "book" <+> unP e
  
--------------------------------------------------------------------------------
-- *** Titles and headers (10.2)

instance CCmdHeader P where
  header a       = P $ cmd "header" <+> unP a 
        
instance CBlock P where
  block e = P $ bracesHanging $ unP e

--------------------------------------------------------------------------------
-- *** Creating titles (10.2.1)

equation :: String -> Doc -> Doc
equation var d = text var <+> equals <+> d

instance CHeaderElement P where
  headerElement e s     = P $ equation e (dquotes $ text s)
  
  breakbefore True      = P $ equation "breakbefore" (text "##t")
  breakbefore False     = P $ equation "breakbefore" (text "##f")

--------------------------------------------------------------------------------    
-- ** MIDI output (10.3)
-- *** Creating MIDI files (10.3.1)

instance CCmdMidi P where
  midi            = P $ cmd "midi"


--------------------------------------------------------------------------------    
-- * Placeholder

instance CPlaceholder P where
  undef          = P $ text "undefined"
  