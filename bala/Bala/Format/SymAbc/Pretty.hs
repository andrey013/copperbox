{-# LANGUAGE MultiParamTypeClasses #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Format.SymAbc.Pretty
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Pretty printer for ABC format
--
--------------------------------------------------------------------------------

module Bala.Format.SymAbc.Pretty where

import Bala.Format.Base.SymBase
import Bala.Format.SymAbc.Datatypes

import Text.PrettyPrint.Leijen



ppfield :: Char -> Doc -> Doc
ppfield ch doc = text [ch,':'] <+> doc




instance SymConcatenation Ctx_Field P where
  (+++) l r    = P $ (unP l) <$> (unP r)  
  
  
instance SymConcatenation Ctx_Element P where
  (+++) l r    = P $ (unP l) <> (unP r)  
 
  


instance SynNumberField P where
  num_ i                = P  $ ppfield 'X' (int i)
  
  
instance SymTextFields P where
  area_ s               = P $ ppfield 'A' (text s)
  book_ s               = P $ ppfield 'B' (text s)
  composer_ s           = P $ ppfield 'C' (text s)
  discography_ s        = P $ ppfield 'D' (text s)
  elemskip_ s           = P $ ppfield 'E' (text s) 
  group_ s              = P $ ppfield 'G' (text s)
  information_ s        = P $ ppfield 'I' (text s)
  notes_ s              = P $ ppfield 'N' (text s)
  origin_ s             = P $ ppfield 'O' (text s)
  rhythm_ s             = P $ ppfield 'R' (text s)
  source_ s             = P $ ppfield 'S' (text s)
  title_ s              = P $ ppfield 'T' (text s)
  words_ s              = P $ ppfield 'W' (text s)
  transcriberNotes_ s   = P $ ppfield 'Z' (text s)

instance SymHistoryField P where
 history_ xs            = P $ ppfield 'H' (align $ vsep $ map text xs)
 
instance SymKeyField P where
  key_ k                = P $ ppfield 'K' (unP k)
  
   
instance SymDefaultLengthField P where
 defaultLength_ r       = P $ ppfield 'L' (pretty r) 
   
instance SymMeterField P where
  meter_ m              = P $ ppfield 'M' (unP m)  

-- simplified
instance SymPartsField P where 
  parts_ cs             = P $ ppfield 'P' (text cs)
  
  
instance SymTempoField P where
  tempo_ t              = P $ ppfield 'Q' (unP t)

  
        
instance SymTempo P where
  tempo i               = P $ int i
  ctempo l i            = P $ group $ char 'C' <> (unP l) <> equals <> int i
  stempo r i            = P $ group $ pretty r <> equals <> int i
              

instance SymLength P where
  ilength i             = P $ int i
  flength r             = P $ pretty r 
  
  
instance SymKey P where
  key s               = P $ unP s
  highlandNoKey       = P $ text "HP" 
  highlandMixolydian  = P $ text "Hp"



instance SymKeySpec P where
  keySpec n           = P $ unP n
      

instance SymAttrMode P where
  mode s a                    = P $ unP a <+> text s
  
instance SymAbcMusic P where
  abcmusic e            = P $ unP e
  
  
instance SymAbcLine P where
  elements e            = P $ unP e
  midtuneField a        = P $ unP a
  
    
instance SymKeyAccidental P where
  keySharp    = P $ char '#' 
  keyFlat     = P $ char 'b'
  
  
instance SymMeter P where
  meter r                     = P $ pretty r
  commonTime                  = P $ char 'C'
  cutTime                     = P $ text "C|"
    
    
instance SymAttrDuration P where
  dur i a                      = P $ group $ unP a <> int i
  
  
instance SymRest P where
  rest                        = P $ char 'z'

 
instance SymAttrOctave P where
  octaveHigh  i a              = P $ group $ unP a <> text (replicate i '\'')
  octaveLow   i a              = P $ group $ unP a <> text (replicate i ',')  
           
             
instance SymAttrAccidental P where 
  natural a                    = P $ group $ char '='  <> unP a 
  sharp a                      = P $ group $ char '^'  <> unP a 
  doubleSharp a                = P $ group $ text "^^" <> unP a 
  flat a                       = P $ group $ char '_'  <> unP a 
  doubleFlat a                 = P $ group $ text "__" <> unP a 


  
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
  



instance SymBaseNote P where
  note p         = P $ pretty p
  
    
instance SymBrokenRhythm P where
  dottedLeft i                = P $ text $ replicate i '>'
  dottedRight i               = P $ text $ replicate i '<'


instance SymTie P where
  tie                         = P $ char '-'
    
instance SymAttrGrace P where
  tilde e                   = P $ group $ char '~' <> unP e 
  stacatto e                = P $ group $ char '.' <> unP e 
  downbow e                 = P $ group $ char 'v' <> unP e 
  upbow e                   = P $ group $ char 'u' <> unP e 


instance SymNPlet P where
  nplet i = P $ group $ char '(' <> int i
        
  
instance SymRepeatMark P where
  repeatMark s  = P $ text s
  
  
instance SymSlur P where
  beginSlur     = P $ lparen
  endSlur       = P $ rparen
  

instance SymAttrGraceNotes P where
  gracenotes xs a  = let gns = braces $ hcat $ map unP xs in
                     P $ gns <> unP a
    
  
  


