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

instance CSnocList P CT_Field where
  snil                  = P $ empty
  snoc xs x             = P $ unP xs <$> unP x 


instance CSnocList P CT_Element where
  snil                  = P $ empty
  snoc xs x             = P $ unP xs <> unP x 
    
instance CAttr P where
  attr e a              = P $ group $ unP e <> unP a 

instance CFieldNumber P where
  num_ i                = P  $ ppfield 'X' (int i)

instance CFieldTitle P where  
  title_ s              = P $ ppfield 'T' (text s)
   
instance CFieldArea P where
  area_ s               = P $ ppfield 'A' (text s)
  
instance CFieldBook P where
  book_ s               = P $ ppfield 'B' (text s)
  
instance CFieldComposer P where
  composer_ s           = P $ ppfield 'C' (text s)

instance CFieldDiscography P where  
  discography_ s        = P $ ppfield 'D' (text s)

instance CFieldElemskip P where  
  elemskip_ s           = P $ ppfield 'E' (text s) 

instance CFieldGroup P where  
  group_ s              = P $ ppfield 'G' (text s)

instance CFieldInformation P where  
  information_ s        = P $ ppfield 'I' (text s)

instance CFieldNotes P where  
  notes_ s              = P $ ppfield 'N' (text s)

instance CFieldOrigin P where  
  origin_ s             = P $ ppfield 'O' (text s)

instance CFieldRhythm P where  
  rhythm_ s             = P $ ppfield 'R' (text s)

instance CFieldSource P where  
  source_ s             = P $ ppfield 'S' (text s)

instance CFieldWords P where  
  words_ s              = P $ ppfield 'W' (text s)

instance CFieldTranscrNotes P where  
  transcrNotes_ s       = P $ ppfield 'Z' (text s)

instance CFieldHistory P where
 history_ xs            = P $ ppfield 'H' (align $ vsep $ map text xs)
 
instance CFieldKey P where
  key_ k                = P $ ppfield 'K' (unP k)
     
instance CFieldDefaultNoteLength P where
 defaultNoteLength_ r   = P $ ppfield 'L' (pretty r) 
   
instance CFieldMeter P where
  meter_ m              = P $ ppfield 'M' (unP m)  

-- simplified
instance CFieldParts P where 
  parts_ cs             = P $ ppfield 'P' (text cs)
  
  
instance CFieldTempo P where
  tempo_ t              = P $ ppfield 'Q' (unP t)

  
        
instance CTempo P where
  tempo i               = P $ int i
  ctempo l i            = P $ group $ char 'C' <> (unP l) <> equals <> int i
  stempo r i            = P $ group $ pretty r <> equals <> int i
              

instance CLength P where
  ilength i             = P $ int i
  flength r             = P $ pretty r 
  
  
instance CKey P where
  key s                 = P $ unP s
  highlandNoKey         = P $ text "HP" 
  highlandMixolydian    = P $ text "Hp"



instance CKeySpec P where
  keySpec n             = P $ unP n
      

instance CMode P where
  mode s                = P $ text s
  
instance CAbcMusic P where
  abcmusic e            = P $ unP e
  
  
instance CAbcLine P where
  elements e            = P $ unP e
  midtuneField a        = P $ unP a
  
    
instance CKeyAccidental P where
  keySharp              = P $ char '#' 
  keyFlat               = P $ char 'b'
  
  
instance CMeter P where
  meter r               = P $ pretty r
  commonTime            = P $ char 'C'
  cutTime               = P $ text "C|"
    
    
instance CDuration P where
  dur i                 = P $ int i
  
  
instance CRest P where
  rest                  = P $ char 'z'

 
instance COctave P where
  octaveHigh  i         = P $ text (replicate i '\'')
  octaveLow   i         = P $ text (replicate i ',')  
           
             
instance CAccidental P where 
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
  



instance CBaseNote P where
  note p                = P $ pretty p
  
    
instance CBrokenRhythm P where
  dottedLeft i          = P $ text $ replicate i '>'
  dottedRight i         = P $ text $ replicate i '<'


instance CTie P where
  tie                   = P $ char '-'
    
instance CGrace P where
  tilde                 = P $ char '~'
  stacatto              = P $ char '.'
  downbow               = P $ char 'v'
  upbow                 = P $ char 'u'


instance CNPlet P where
  nplet i               = P $ group $ char '(' <> int i
        
  
instance CRepeatMark P where
  repeatMark s          = P $ text s
  
  
instance CSlur P where
  beginSlur             = P $ lparen
  endSlur               = P $ rparen
  

instance CGraceNotes P where
  gracenotes xs         = P $  braces $ hcat $ map unP xs
    
  
  


