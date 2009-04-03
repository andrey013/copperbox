{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.AbcForm
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Building note lists
--
--
--------------------------------------------------------------------------------

module HNotate.AbcForm where

import HNotate.Cardinal
import HNotate.Duration hiding ( spacer )
import HNotate.MusicRepDatatypes
import qualified HNotate.NoteList as N
import HNotate.Pitch
import HNotate.Staff
import HNotate.Utils ( prime )


import Data.Ratio
import Text.PrettyPrint.Leijen

type Multiplier = Rational

-- The Element datatype - represents elements with a 'unit duration'.
-- E.g a chord has a set of pitches but the unit duration is common to all 
-- of them. 
data Element = 
      Note 
        { note_pitch          :: Pitch
        , unl_multiplier      :: Multiplier
        }                  
    | Rest  
        { unl_multiplier      :: Multiplier }
    | Spacer  
        { unl_multiplier      :: Multiplier}
    | Chord 
        { chord_elements      :: [Pitch] 
        , unl_multiplier      :: Multiplier
        }          
    | GraceNotes 
        { grace_elements      :: [GraceNote] }                              
    | Nplet 
        { nplet_multipier     :: Int
        , nplet_elements      :: [Pitch] 
        }                   
  deriving (Show) 

type GraceNote = Pitch

unitRescale :: Duration -> Duration -> Duration
unitRescale unl drn = (dn%dd) / (un%ud) where
    (dn,dd)  = ratioElements drn
    (un,ud)  = ratioElements unl

abcStaff :: LabelSet -> Duration -> Staff N.Element -> Staff Element
abcStaff ls u (Staff xs) = Staff (fmap (fmap (fmap (abcElement ls u))) xs) 

    
abcElement :: LabelSet -> Duration -> N.Element -> Element
abcElement ls u (N.Note p d)      = Note (naturalize ls p) (unitRescale u d)
abcElement _  u (N.Rest d)        = Rest (unitRescale u d) 
abcElement _  u (N.Spacer d)      = Spacer (unitRescale u d) 
abcElement ls u (N.Chord ps d)    = Chord (map (naturalize ls) ps) (unitRescale u d)        
abcElement ls _ (N.GraceNotes ns) = GraceNotes (map (naturalize ls . fst) ns)                          
abcElement ls _ (N.Nplet m _ ps)  = Nplet m (map (naturalize ls) ps)

--------------------------------------------------------------------------------
-- output

outputAbc :: Staff Element -> Doc
outputAbc = vsep . map (\a -> overlay a <+> char '|') . getStaff 

overlay :: Cardinal (Bar Element) -> Doc
overlay (Single x) = bar x
overlay (Multi xs) = hsep $ punctuate (text "&\\") (map bar xs)


bar :: Bar Element -> Doc 
bar (Bar xs)        = hsep $ map beamGroup xs
bar (TiedBar x xs)  = char '-' <> element x <+> (hsep $ map beamGroup xs)


beamGroup :: BeamGroup Element -> Doc
beamGroup (Single e) = element e
beamGroup (Multi es) = hcat $ map element es

-- Elements - rest 'r', spacer 'x', chord [pitch+], grace notes {pitch+}

element :: Element -> Doc
element (Note p m)      = note p m
element (Rest m)        = rest m 
element (Spacer m)      = spacer m 
element (Chord ps m)    = brackets (hcat $ map pitch ps) <> multiplier m        
element (GraceNotes ps) = braces (hcat $ map pitch ps)                             
element (Nplet m ps)    = char '(' <> int m <+> (hcat $ map pitch ps)


note :: Pitch -> Multiplier -> Doc 
note p m = pitch p <> multiplier m


rest :: Multiplier -> Doc
rest m = char 'z' <> multiplier m
    
spacer :: Multiplier -> Doc
spacer m = char 'x' <> multiplier m


data PitchChar = UPPER | LOWER
  deriving (Eq,Show)
  
pitch :: Pitch -> Doc
pitch (Pitch l a o) 
    | o > 4     = pitchLabel (PitchLabel l a) LOWER <> octave o 
    | otherwise = pitchLabel (PitchLabel l a) UPPER <> octave o 
  where
    octave :: Int -> Doc
    octave i  | i > 5       = text (replicate (i-5) '\'') 
              | i < 4       = text (replicate (4-i) ',')
              | otherwise   = empty


pitchLabel :: PitchLabel -> PitchChar -> Doc
pitchLabel (PitchLabel l a) pc 
    | pc == LOWER   = accidental a <> (char . toLowerLChar) l
    | otherwise     = accidental a <> (char . toUpperLChar) l
  where     
    accidental :: Accidental -> Doc
    accidental Nat           = empty    
    accidental Sharp         = char '^' 
    accidental Flat          = char '_' 
    accidental DoubleSharp   = text "^^"
    accidental DoubleFlat    = text "__"

multiplier :: Duration -> Doc
multiplier dn | dn == 1   = empty
              | otherwise = fn $ ratioElements $ convRational dn
  where
    fn (n,1) = int n
    fn (1,d) = char '/' <> int d
    fn (n,d) = int n <> char '/' <> int d
    
    
--------------------------------------------------------------------------------
-- pretty print

instance Pretty Element where
  pretty (Note p m)           = pretty p <> prime <> ppMultiplier m
  pretty (Rest m)             = char 'r' <> ppMultiplier m 
  pretty (Spacer m)           = char 's' <> ppMultiplier m
  pretty (Chord se m)         = brackets (hsep $ fmap pretty se) 
                                    <> prime <> ppMultiplier m
  pretty (GraceNotes se)      = braces (hsep $ fmap pretty se)
  pretty (Nplet _ se)       = braces (hsep $ fmap pretty se)    

ppMultiplier :: Multiplier -> Doc
ppMultiplier m | m == 1     = empty
           | otherwise  = text $ show m


