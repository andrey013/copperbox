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
  pretty MarkTie      = char '~'
  pretty (MarkSlur a) = pretty a
  pretty (MarkBeam a) = pretty a 

          
          
instance Pretty Slur where
  pretty (SlurStart ov) = group $ pretty ov <> char '('
  pretty SlurEnd        = char ']'


instance Pretty Beam where
  pretty BeamStart = char '['
  pretty BeamEnd   = char ']'
          
            
cmd :: String -> Doc
cmd = string . ('\\' :)


instance Pretty Command where
  pretty (CmdKey p m) = cmd "key" <+> pretty p <+> pretty m
  pretty (CmdVersion xs) = 
    cmd "version" <+> string (concat $ intersperse "." (map show xs))
    
  pretty (CmdTimeSignature n d) = 
    cmd "time" <+> int n <> char '/' <> int d
  
  pretty (CmdTempo d v) = cmd "tempo" <+> pretty d <> equals <> int v
    
  
  pretty (NullaryCommand name) = cmd name
  
  pretty (ExprCommand name water) = cmd name <+> braces (string water)
  
  pretty (UnaryCommand name s) = cmd name <+> string s
  
  pretty (BinaryCommand name s s') = cmd name <+> string s <+> string s'
  

ppGuitarNote :: (Pitch,Int) -> Doc
ppGuitarNote (p,i) = pretty p <> ppGuitarString i

ppGuitarString :: Int -> Doc
ppGuitarString = string .  ('\\' :) .  show

             