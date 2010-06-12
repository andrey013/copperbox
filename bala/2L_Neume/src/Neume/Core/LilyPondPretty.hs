{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Core.LilyPondPretty
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Pretty printers for LilyPond.
--
--------------------------------------------------------------------------------


module Neume.Core.LilyPondPretty
  (
  -- * Printing glyphs
    note
  , pitch
  , pitchLabel
  , duration
  , rest
  , spacer
  , tie

  , chordForm
  , graceForm
  , beamForm
  , pletForm

  -- * Pretty printers
  , doubleBar 
  , singleBar

  , repeatvolta
  , alternative

  , parallelMusic

  ) where


import Neume.Core.Duration
-- import Neume.Core.Metrical
import Neume.Core.Pitch
import Neume.Core.ModularSyntax ( PletMult )
import Neume.Core.Utils.Pretty

import Text.PrettyPrint.Leijen                  -- package: wl-pprint

lyCommand :: String -> Doc
lyCommand = (char '\\' <>) . text 



--------------------------------------------------------------------------------
-- Printing glyphs

-- | Print a note, the duration is a Maybe value. Nothing indicates
-- that the note has the same duration as the previous glyph.
note :: Pitch -> Maybe Duration -> Doc
note p md = pitch p <> maybe empty duration md

-- | Print a Pitch.
pitch :: Pitch -> Doc 
pitch pch@(Pitch _ _ o) = pitchLabel (label pch) <> ove o where
    ove i | i > 0       = text $ replicate i       '\''
          | i < 0       = text $ replicate (abs i) ','
          | otherwise   = empty


-- | Print a 'PitchLabel'.
pitchLabel :: PitchLabel -> Doc
pitchLabel (PitchLabel l ma) = 
    char (toLowerLChar l) <> maybe empty accidental ma
  where 
    accidental :: Accidental -> Doc
    accidental Nat            = text "!"    -- check correctness
    accidental Sharp          = text "is"
    accidental Flat           = text "es"
    accidental DoubleSharp    = text "isis"
    accidental DoubleFlat     = text "eses"

-- | Print a Duration.
duration :: Duration -> Doc
duration = maybe empty df . lyRepresentation
  where
    df (LyCmd ss,dc) = dots dc $ lyCommand ss 
    df (LyNum i,dc)  = dots dc $ int i

    dots :: Int -> (Doc -> Doc)
    dots i | i > 0     = (<> text (replicate i '.'))
           | otherwise = id



-- | Print a rest, duration is a Maybe value - Nothing indicates
-- that the rest has the same duration as the previous glyph.  
rest :: Maybe Duration -> Doc
rest md = char 'r' <> maybe empty duration md


-- | Print an invisible rest, commonly used to align overlayed 
-- bars. Duration is a Maybe value - Nothing indicates
-- that the spacer has the same duration as the previous glyph.  
spacer :: Maybe Duration -> Doc
spacer md = char 's' <> maybe empty duration md

-- | Print a tie.
tie :: Doc
tie = char '~'


-- | Chords - notes printed inside angle brackets, followed by 
-- duration, e.g.:
--
-- @ 
--  \<c e g\>4
-- @ 
chordForm :: [Doc] -> Maybe Duration -> Doc
chordForm xs md = angles (hsep xs) <> maybe empty duration md


-- | Grace notes - @\\grace@ command then expression inside braces,
-- e.g:
--
-- @ 
--  \\grace { f32[ e] }
-- @ 
graceForm :: [Doc] -> Doc
graceForm [x] = lyCommand "grace" <+> braces x where
graceForm xs  = lyCommand "grace" <+> braces (beamForm xs)

  
-- | Beams - first element printed outside the square brackets, e.g.:
-- @ 
--  c16 [e g c]
-- @ 
beamForm :: [Doc] -> Doc
beamForm (x:xs) = x <> char '[' <+> hsep xs <> char ']'
beamForm []     = emptyDoc



-- | N-ary plets - @\\times ../..@ command then expression 
-- inside braces,
-- e.g:
--
-- @ 
--  \\times 3/5 { c8[ c c c c] }
-- @ 
pletForm :: PletMult -> [Doc] -> Doc
pletForm (n,d) xs = lyCommand "times" <+> integer n <> char '/' <> integer d
                                      <+> braces (hsep xs)




--------------------------------------------------------------------------------



-- Bar lines

-- | Print a double bar line @||@.
doubleBar :: Doc 
doubleBar = lyCommand "bar" <+> dquotes (text "||")

-- | Print a single bar line @|@.
singleBar :: Doc
singleBar = text "|"


-- Repeats

-- | @\\repeat volta n {\\n ... \\n}@ - print a repeated block.
--
repeatvolta :: Int -> Doc -> Doc 
repeatvolta i expr = 
    lyCommand "repeat" <+> text "volta" <+> int i <+> nestBraces expr

-- | @\\alternative { \\n { ... } \\n { ... } ... }@
--
alternative :: [Doc] -> Doc
alternative = (lyCommand "alternative" <+>) . nestBraces . vsep . map braces 



-- | @\\parallelMusic #'( ... ) {\\n ...\\n }@.
parallelMusic       :: [String] -> Doc -> Doc
parallelMusic xs e  = 
    lyCommand "parallelMusic" <+> text "#'" <> parens names <+> nestBraces e
  where
    names = hsep $ map text xs
