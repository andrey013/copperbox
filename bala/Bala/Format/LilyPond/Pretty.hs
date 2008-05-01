--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Format.LilyPond.Pretty
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

module Bala.Format.LilyPond.Pretty where

import Bala.Format.LilyPond.Datatypes

import qualified Bala.Base.BaseExtra as Ex

import Text.PrettyPrint.Leijen

import Data.List hiding (group)


ppLilyPondFile :: LilyPondFile -> Doc
ppLilyPondFile = vsep . map pretty


expr :: Doc -> Doc
expr d = lbrace <+> d <+> rbrace 

instance Pretty Glyphs where
  pretty (Glyphs xs) = hsep (map pretty xs)

instance Pretty Glyph where
  pretty (GlyphEvent a)     = pretty a 
  pretty (GlyphCommmand a)  = pretty a
  pretty (GlyphMark a)      = pretty a
           
instance Pretty GlyphEvent where
  pretty (GEvtNote a)       = pretty a 
  pretty (GEvtChord a)      = pretty a 
  pretty (GEvtRest a)       = pretty a 

instance Pretty Note where
  pretty (Note p od) = group (pretty p <> pretty od) 
                
instance Pretty Rest where
  pretty (Rest a) = group (char 'r' <> pretty a)

  
                             
ppStringMark :: String -> Doc
ppStringMark ss = char '^' <> dquotes (text ss)

instance Pretty Duration where
  pretty (Duration i) = int i
  pretty (Dotted i)   = string ((shows i . Ex.dotS) []) 

instance Pretty Pitch where
  pretty (Pitch l oa oo)  = char l <> pretty oa <> pretty oo

instance Pretty Accidental where
  pretty Sharp        = string "is"
  pretty Flat         = string "es"
  pretty DoubleSharp  = string "isis"
  pretty DoubleFlat   = string "eses"

  

instance Pretty OctaveSpec where
  pretty (Raised i)  = string (replicate i '\'')
  pretty (Lowered i) = string (replicate i ',')

instance Pretty Articulation where
  pretty (Articulation ap ch) = pretty ap <> char ch
  pretty (Fingering ap i)     = pretty ap <> int i
  
instance Pretty VerticalPlacement where
  pretty VAbove   = char '^'
  pretty VBelow   = char '_'
  pretty VDefault = char '-'

instance Pretty Chord where
  pretty (Chord ps d) = angles (hsep $ map pretty ps) <> pretty d

-- marks
instance Pretty  Mark where
  pretty MarkTie        = char '~'
  pretty (MarkSlur a)   = pretty a
  pretty (MarkBeam a)   = pretty a 
  pretty MarkBarCheck   = char '|'
          
          
instance Pretty Slur where
  pretty (SlurStart ov) = group $ pretty ov <> char '('
  pretty SlurEnd        = char ']'


instance Pretty Beam where
  pretty BeamStart = char '['
  pretty BeamEnd   = char ']'
          
            
cmd :: String -> Doc
cmd = string . ('\\' :)


instance Pretty Command where
  pretty (CmdNew d) = cmd "new" <+> pretty d

  pretty (CmdKey p m) = cmd "key" <+> pretty p <+> pretty m
  pretty (CmdVersion xs) = 
    cmd "version" <+> string (concat $ intersperse "." (map show xs))
    
  pretty (CmdTimeSignature n d) = 
    cmd "time" <+> int n <> char '/' <> int d
  
  pretty (CmdTempo d v) = cmd "tempo" <+> pretty d <> equals <> int v
  
  pretty (CmdAddlyrics s) = cmd "addlyrics" <+> expr (string s)  
  
  pretty (CmdDrums ds) = cmd "drums" <+> expr (hsep $ map pretty ds)
  
  pretty (NullaryCommand name) = cmd name
  
  pretty (ExprCommand name water) = cmd name <+> braces (string water)
  
  pretty (UnaryCommand name s) = cmd name <+> string s
  
  pretty (BinaryCommand name s s') = cmd name <+> string s <+> string s'


instance Pretty NewDecl where
  pretty (NewTabStaff xs) = text "TabStaff" <+> encloseSep lbrace rbrace space ds
    where ds = map pretty xs  

ppGuitarNote :: (Pitch,Int) -> Doc
ppGuitarNote (p,i) = pretty p <> ppGuitarString i

ppGuitarString :: Int -> Doc
ppGuitarString = string .  ('\\' :) .  show


--------------------------------------------------------------------------------
-- Chordmode
--------------------------------------------------------------------------------

instance Pretty Chordname where  
  pretty (Chordname n Nothing)    = pretty n
  pretty (Chordname n (Just s))   = group $ pretty n <> colon <> pretty s

instance Pretty ChordSuffix where
  pretty (ChordSuffix oi cm []) = ppType oi <> pretty cm
  pretty (ChordSuffix oi cm xs) = ppType oi <> pretty cm <> hcat (map pretty xs)

ppType (Just a) = ppAltint a
ppType Nothing  = empty

ppAltint (i,Nothing) = int i
ppAltint (i, Just a) = int i <> pretty a

            
instance Pretty ChordModifier where  
  pretty CModM        = empty
  pretty CModMinor    = char 'm'
  pretty CModDim      = string "dim"
  pretty CModAug      = string "aug"
  pretty CModMaj7     = string "maj"
  pretty (CModSus i)  = string "sus" <> int i


instance Pretty ChordStep where  
  pretty (CStepAdd i oa)  = char '.' <> int i <> pretty oa
  pretty (CStepRemove i)  = char '^' <> int i 

  
instance Pretty ChordAlt where  
  pretty CAltPlus       = char '+'
  pretty CAltMinus      = char '-'
              
--------------------------------------------------------------------------------
-- guitar
--------------------------------------------------------------------------------

instance Pretty GuitarNote where
  pretty (GuitarNote n i ) = group $ pretty n <> char '\\' <> int i
  
--------------------------------------------------------------------------------
-- vocal
--------------------------------------------------------------------------------

instance Pretty LyricContent where
  pretty (Syllable cs k oi) = group $ text cs <> pretty k <> pretty oi
  pretty CenteredHyphen     = text "--"
  pretty ExtenderLine       = text "__"

  
instance Pretty SyllableCont where
  pretty SyllableCont   = char '-'
  pretty NoSyllableCont = empty
  
  
--------------------------------------------------------------------------------
-- vocal
--------------------------------------------------------------------------------

instance Pretty Drumnote where
  pretty (Drumnote s od) = group $ text s <> pretty od


