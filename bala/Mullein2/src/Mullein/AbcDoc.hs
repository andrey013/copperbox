{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.AbcDoc
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Pretty printers for ABC.
--
--------------------------------------------------------------------------------


module Mullein.AbcDoc
  (
  -- * Printing glyphs
    note
  , pitch
  , pitchLabel
  , pitchLabelLower
  , multiplier
  , spacer
  , rest
  , chordForm
  , graceForm


  -- * ABC literals and syntax
  , singleBar
  , overlay
  , tie
  
  -- *** Fields

  , field
  , tunenum  
  , title
  , book 
  , composer
  , meter
  , tempo
  , key

  ) where


import Mullein.Duration
import Mullein.Pitch

import Text.PrettyPrint.Leijen


--------------------------------------------------------------------------------
-- Printing glyphs

-- | Print a note. Note that durations in ABC are multipliers of
-- the /unit note length/ rather than absolute values. 
note :: Pitch -> AbcMultiplier -> Doc 
note p m = pitch p <> multiplier m


data PitchChar = UPPER | LOWER
  deriving (Eq,Show)


-- | Print a Pitch. Middle c in ABC is @C@ - upper case c. Notes in 
-- higher octaves uses lower case letters. 
pitch :: Pitch -> Doc
pitch (Pitch l a o) 
    | o > 5     = pitchLabel' (PitchLabel l a) LOWER <> ove o 
    | otherwise = pitchLabel' (PitchLabel l a) UPPER <> ove o 
  where
    ove :: Int -> Doc
    ove i  | i > 6       = text (replicate (i-6) '\'') 
              | i < 5       = text (replicate (5-i) ',')
              | otherwise   = empty


-- | Print a 'PitchLabel' - the pitch letter will be printed in 
-- upper case. 
pitchLabel :: PitchLabel -> Doc
pitchLabel = pitchLabel' `flip` UPPER



-- | Print a 'PitchLabel' - the pitch letter will be printed in 
-- lower case. 
pitchLabelLower :: PitchLabel -> Doc
pitchLabelLower = pitchLabel' `flip` LOWER

pitchLabel' :: PitchLabel -> PitchChar -> Doc
pitchLabel' (PitchLabel l a) pc 
    | pc == LOWER   = (maybe empty accidental a) <> (char . toLowerLChar) l
    | otherwise     = (maybe empty accidental a) <> (char . toUpperLChar) l
  where     
    accidental :: Accidental -> Doc
    accidental Nat           = char '='    
    accidental Sharp         = char '^' 
    accidental Flat          = char '_' 
    accidental DoubleSharp   = text "^^"
    accidental DoubleFlat    = text "__"

-- | Print a duration multiplier.
multiplier :: AbcMultiplier -> Doc
multiplier IdenM      = empty
multiplier (Mult n)   = integer n
multiplier (Div n)    = char '/' <> integer n
multiplier (Frac n d) = integer n <> char '/' <> integer d


-- | Print a rest.
rest :: AbcMultiplier -> Doc
rest dm = char 'z' <> multiplier dm

-- | Print an invisible rest, commonly used to align overlayed 
-- bars.
spacer :: AbcMultiplier -> Doc
spacer dm      = char 'x' <> multiplier dm


-- | Chords - notes are printed inside square brackets, e.g.:
-- @ 
--  [c4e4g4]
-- @ 
chordForm :: [Doc] -> Doc
chordForm = brackets . hcat


-- | Grace notes are printed inside braces,
-- e.g:
-- @ 
--  {f2e}
-- @ 
--
-- Not all ABC processors acknowledge duration multipliers within
-- graces.
graceForm :: [Doc] -> Doc
graceForm = braces . hcat


--------------------------------------------------------------------------------
-- ABC literals and syntax


singleBar :: Doc
singleBar = char '|'

overlay :: [Doc] -> Doc
overlay = vsep . punctuate (text " & ")    


tie :: Doc
tie = char '~'


-- ** Fields

field :: Char -> Doc -> Doc
field ch d = char ch <> colon <> d


-- | @X field@ - reference \/ tune number.
tunenum :: Int -> Doc
tunenum = field 'X' . int

-- | @T field@ - title. 
title :: String -> Doc
title = field 'T' . text

-- | @B field@ - book.
book :: String -> Doc
book = field 'B' . text

-- | @C field@ - composer name.
composer :: String -> Doc
composer = field 'C' . text

-- | @M field@ - meter.
-- Note - the meter parameter should correspond to the meter component
-- of the @MetricalSpec@ used to generate the ABC tune.
meter :: String -> Doc
meter = field 'M' . text

-- | @Q field@ - tempo.
-- Note - the range of ABC tempos is very wide, therefore no attempt
-- is made to encapsulate /tempo/ as an abstract datatype. Instead
-- tempo is just a string literal. If you want to use a genuine literal
-- (e.g. Andante) make sure you double-quote it first.
tempo :: String -> Doc
tempo = field 'Q' . text

-- | @K field@ - key.
-- Note - the key parameter should correspond to the key used to 
-- generate the ABC tune.
key :: String -> Doc
key = field 'K' . text  




