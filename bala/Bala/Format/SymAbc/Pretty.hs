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

import Text.PrettyPrint.Leijen



ppfield :: Char -> Doc -> Doc
ppfield ch doc = text [ch,':'] <+> doc

instance SymCList P CT_Field where
  cNil                  = P $ empty
  cSnoc xs x            = P $ unP xs <$> unP x 


instance SymCList P CT_Element where
  cNil                  = P $ empty
  cSnoc xs x            = P $ unP xs <> unP x 
    
instance SymAttr P where
  attr e a              = P $ group $ unP e <> unP a 

instance SymFieldNumber P where
  num_ i                = P  $ ppfield 'X' (int i)

instance SymFieldTitle P where  
  title_ s              = P $ ppfield 'T' (text s)
   
instance SymFieldArea P where
  area_ s               = P $ ppfield 'A' (text s)
  
instance SymFieldBook P where
  book_ s               = P $ ppfield 'B' (text s)
  
instance SymFieldComposer P where
  composer_ s           = P $ ppfield 'C' (text s)

instance SymFieldDiscography P where  
  discography_ s        = P $ ppfield 'D' (text s)

instance SymFieldElemskip P where  
  elemskip_ s           = P $ ppfield 'E' (text s) 

instance SymFieldGroup P where  
  group_ s              = P $ ppfield 'G' (text s)

instance SymFieldInformation P where  
  information_ s        = P $ ppfield 'I' (text s)

instance SymFieldNotes P where  
  notes_ s              = P $ ppfield 'N' (text s)

instance SymFieldOrigin P where  
  origin_ s             = P $ ppfield 'O' (text s)

instance SymFieldRhythm P where  
  rhythm_ s             = P $ ppfield 'R' (text s)

instance SymFieldSource P where  
  source_ s             = P $ ppfield 'S' (text s)

instance SymFieldWords P where  
  words_ s              = P $ ppfield 'W' (text s)

instance SymFieldTranscrNotes P where  
  transcrNotes_ s       = P $ ppfield 'Z' (text s)

instance SymFieldHistory P where
 history_ xs            = P $ ppfield 'H' (align $ vsep $ map text xs)
 
instance SymFieldKey P where
  key_ k                = P $ ppfield 'K' (unP k)
     
instance SymFieldDefaultNoteLength P where
 defaultNoteLength_ r   = P $ ppfield 'L' (pretty r) 
   
instance SymFieldMeter P where
  meter_ m              = P $ ppfield 'M' (unP m)  

-- simplified
instance SymFieldParts P where 
  parts_ cs             = P $ ppfield 'P' (text cs)
  
  
instance SymFieldTempo P where
  tempo_ t              = P $ ppfield 'Q' (unP t)

  
        
instance SymTempo P where
  tempo i               = P $ int i
  ctempo l i            = P $ group $ char 'C' <> (unP l) <> equals <> int i
  stempo r i            = P $ group $ pretty r <> equals <> int i
              

instance SymLength P where
  ilength i             = P $ int i
  flength r             = P $ pretty r 
  
  
instance SymKey P where
  key s                 = P $ unP s
  highlandNoKey         = P $ text "HP" 
  highlandMixolydian    = P $ text "Hp"



instance SymKeySpec P where
  keySpec n             = P $ unP n
      

instance SymMode P where
  mode s                = P $ text s
  
instance SymAbcMusic P where
  abcmusic e            = P $ unP e
  
  
instance SymAbcLine P where
  elements e            = P $ unP e
  midtuneField a        = P $ unP a
  
    
instance SymKeyAccidental P where
  keySharp              = P $ char '#' 
  keyFlat               = P $ char 'b'
  
  
instance SymMeter P where
  meter r               = P $ pretty r
  commonTime            = P $ char 'C'
  cutTime               = P $ text "C|"
    
    
instance SymDuration P where
  dur i                 = P $ int i
  
  
instance SymRest P where
  rest                  = P $ char 'z'

 
instance SymOctave P where
  octaveHigh  i         = P $ text (replicate i '\'')
  octaveLow   i         = P $ text (replicate i ',')  
           
             
instance SymAccidental P where 
  natural               = P $ char '='
  sharp                 = P $ char '^'
  doubleSharp           = P $ text "^^"
  flat                  = P $ char '_'
  doubleFlat            = P $ text "__"


  
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
  note p                = P $ pretty p
  
    
instance SymBrokenRhythm P where
  dottedLeft i          = P $ text $ replicate i '>'
  dottedRight i         = P $ text $ replicate i '<'


instance SymTie P where
  tie                   = P $ char '-'
    
instance SymGrace P where
  tilde                 = P $ char '~'
  stacatto              = P $ char '.'
  downbow               = P $ char 'v'
  upbow                 = P $ char 'u'


instance SymNPlet P where
  nplet i               = P $ group $ char '(' <> int i
        
  
instance SymRepeatMark P where
  repeatMark s          = P $ text s
  
  
instance SymSlur P where
  beginSlur             = P $ lparen
  endSlur               = P $ rparen
  

instance SymGraceNotes P where
  gracenotes xs         = P $  braces $ hcat $ map unP xs
    
  
  


