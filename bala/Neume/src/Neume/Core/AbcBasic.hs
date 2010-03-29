{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Core.AbcBasic
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Pretty printers for ABC.
--
--------------------------------------------------------------------------------


module Neume.Core.AbcBasic
  (
  -- * Printing glyphs
    note
  , pitch
  , pitchLabel
  , pitchLabelLower
  , multiplier
  , spacer
  , rest
  , tie
  , chordForm
  , graceForm
  , pletContext

  ) where


import Neume.Core.Duration
import Neume.Core.Pitch

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
    | o > 4     = pitchLabel' (PitchLabel l a) LOWER <> ove o 
    | otherwise = pitchLabel' (PitchLabel l a) UPPER <> ove o 
  where
    ove :: Int -> Doc
    ove i  | i > 5       = text (replicate (i-5) '\'') 
           | i < 4       = text (replicate (4-i) ',')
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


tie :: Doc
tie = char '~'


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

-- | @(p:q:r@
--
-- @p@ notes in the time of @r@ for the next @q@ notes.
--
pletContext :: (Int,Int,Int) -> Doc
pletContext (p,q,r) = lparen <> int p <> colon <> int q <> colon <> int r
