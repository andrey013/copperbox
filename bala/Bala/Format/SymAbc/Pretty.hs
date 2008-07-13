{-# LANGUAGE MultiParamTypeClasses #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Format.SymAbc.Pretty
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  multi-parameter typeclasses
--
-- Pretty printer for ABC format
--
--------------------------------------------------------------------------------

module Bala.Format.SymAbc.Pretty where

import Bala.Format.Base.SymBase
import Bala.Format.SymAbc.Datatypes
import Bala.Base.Meter

import Data.Ratio
import Text.PrettyPrint.Leijen



ppfield :: Char -> Doc -> Doc
ppfield ch doc = text [ch,':'] <+> doc

instance CSnocList P CT_Field where
  snil                  = P $ empty
  snoc xs x             = P $ unP xs <$> unP x 


instance CSnocList P CT_Element where
  snil                  = P $ empty
  snoc xs x             = P $ unP xs <+> unP x 
    
-- instance CAttr P where
--  attr e a              = P $ group $ unP e <> unP a 


--------------------------------------------------------------------------------
-- * Information fields (3)

instance CField P where
  area_field s                    = P $ ppfield 'A' (text s)
  book_field s                    = P $ ppfield 'B' (text s)
  composer_field s                = P $ ppfield 'C' (text s)  
  discography_field s             = P $ ppfield 'D' (text s)
  group_field s                   = P $ ppfield 'G' (text s)
  history_field xs                = P $ ppfield 'H' (align $ vsep $ map text xs)  
  information_field s             = P $ ppfield 'I' (text s)
  notes_field s                   = P $ ppfield 'N' (text s)
  origin_field s                  = P $ ppfield 'O' (text s)  
  rhythm_field s                  = P $ ppfield 'R' (text s)  
  source_field s                  = P $ ppfield 'S' (text s)    
  transcriber_notes_field s       = P $ ppfield 'Z' (text s) 
  number_field i                  = P $ ppfield 'X' (int i)




instance CMidTuneField P where
  elemskip_field s                = P $ ppfield 'E' (text s)
  key_field k                     = P $ ppfield 'K' (unP k)
  default_note_length_field r     = P $ ppfield 'L' (pretty r)
  meter_field m                   = P $ ppfield 'M' (unP m)
  parts_field cs                  = P $ ppfield 'P' (text cs)
  tempo_field t                   = P $ ppfield 'Q' (unP t)
  title_field s                   = P $ ppfield 'T' (text s)
  words_field s                   = P $ ppfield 'W' (text s)
   
-- ** M: meter (3.1.6)
instance CMeter P where
  meter r               = P $ pretty r
  commonTime            = P $ char 'C'
  cutTime               = P $ text "C|"
  
    
-- ** Q: tempo (3.1.8)
instance CTempo P where
  tempo i               = P $ int i
  ctempo l i            = P $ group $ char 'C' <> (unP l) <> equals <> int i
  stempo r i            = P $ group $ pretty r <> equals <> int i
              

instance CLength P where
  ilength i             = P $ int i
  flength r             = P $ pretty r 

  
-- ** K: key (3.1.14)
instance CKey P where
  key s                 = P $ unP s
  highlandNoKey         = P $ text "HP" 
  highlandMixolydian    = P $ text "Hp"
  
instance CKeySpec P where
  keySpec n             = P $ unP n
        
instance CKeyAccidental P where
  keySharp              = P $ char '#' 
  keyFlat               = P $ char 'b'
  
instance CMode P where
  mode s                = P $ text s
  
  
--------------------------------------------------------------------------------
-- * The tune body (4)
instance CTuneBody P where
  tunebody e            = P $ unP e
  
  
instance CAbcLine P where
  elements e            = P $ unP e
  midtuneField a        = P $ unP a
  

  
-- ** Pitch (4.1)
instance Pretty PitchLetter where
  pretty C    = char 'C'
  pretty D    = char 'D'
  pretty E    = char 'E'
  pretty F    = char 'F'
  pretty G    = char 'G'
  pretty A    = char 'A'
  pretty B    = char 'B'  
  pretty C2   = char 'c'
  pretty D2   = char 'd'
  pretty E2   = char 'e'
  pretty F2   = char 'f'
  pretty G2   = char 'g'
  pretty A2   = char 'a'
  pretty B2   = char 'b'  


instance CBaseNote P where
  note p                = P $ pretty p

 
instance COctave P where
  octaveHigh  i         = P $ text (replicate i '\'')
  octaveLow   i         = P $ text (replicate i ',')  
           

-- ** Accidentals (4.2)             
instance CAccidental P where 
  natural               = P $ char '='
  sharp                 = P $ char '^'
  doubleSharp           = P $ text "^^"
  flat                  = P $ char '_'
  doubleFlat            = P $ text "__"

-- ** Note lengths (4.3)
instance CDuration P where
  dur mf          = let (n,d) = unMeterFraction mf in P $ ppRatio (n%d)

ppRatio r = let (n,d) = (numerator r, denominator r) in f n d
  where 
    f 1 1   = empty       -- the default note length
    f n 1   = int n
    f 1 2   = char '/'    -- short hand for pitch/2
    f n d   = group $ ppRatio (n%d)
  
    

  
-- ** Broken rhythm (4.4)
instance CBrokenRhythm P where
  dottedLeft i          = P $ text $ replicate i '>'
  dottedRight i         = P $ text $ replicate i '<'
  
-- ** Rests (4.5)
instance CRest P where
  rest                  = P $ char 'z'

-- ** Repeat \/ bar symbols (4.8)  
instance CRepeatMark P where
  repeatMark s          = P $ text s
    

-- ** Ties and slurs (4.11) 
instance CTie P where
  tie                   = P $ char '-'

instance CSlur P where
  beginSlur             = P $ lparen
  endSlur               = P $ rparen
  
-- ** Grace notes (4.12)
instance CGraceNotes P where
  gracenotes xs         = P $  braces $ hcat $ map unP xs
  


-- ** Duplets, triplets, quadruplets, etc. (4.13)
instance CNPlet P where
  nplet i               = P $ group $ char '(' <> int i


-- ** Decorations (4.13)
instance CDecoration P where
  tilde                 = P $ char '~'
  stacatto              = P $ char '.'
  downbow               = P $ char 'v'
  upbow                 = P $ char 'u'

-- ** Chords and unisons (4.17)
instance CChord P where
  chord xs              = P $ (braces $ hsep $ map unP xs)

-- * Multiple voices (7)
-- ** Voice overlay (7.4)
  
instance CVoiceOverlay P where
  (&\) a b              = P $ unP a <+> text "\\&" <+> unP b


