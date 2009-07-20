{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.LilyPondOutput
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Pretty print LilyPond
--
--------------------------------------------------------------------------------


module Mullein.LilyPondOutput  where

import Mullein.Core
import Mullein.Duration
import Mullein.Pitch
import Mullein.Utils


import Data.OneMany

import Text.PrettyPrint.Leijen


class LyOutput e where
  type LyDur e :: *
  lyNote  :: e -> LyDur e -> Doc
  lyPitch :: e -> Doc


class LilyPondGlyph e where
  lyGlyph :: e -> Doc

instance LilyPondGlyph (ElementP ScNote) where
  lyGlyph = oElement  

instance LyOutput Pitch where
  type LyDur Pitch = Duration
  lyNote p od = note p <> optDuration od
  lyPitch = note

instance LyOutput ScNote where
  type LyDur ScNote = Duration
  lyNote (ScNote p _) od = note p <> optDuration od
  lyPitch (ScNote p _) = note p



-- oBar 

oBarOverlay :: LilyPondGlyph e => (Bool,[OneMany e]) -> Doc
oBarOverlay (ptied,xs) = hsep (map omBeam xs) <> if ptied then char '~' else empty


omBeam :: LilyPondGlyph e => OneMany e -> Doc
omBeam = oneMany lyGlyph (lyBeam . map lyGlyph) 

oBracket :: (LyOutput e, LyDur e ~ Duration) => OneMany (ElementP e) -> Doc
oBracket = oneMany oElement (lyBeam . map oElement)


-- TODO check whether or not successive notes in chords and graces
-- change the relative pitch

oElement :: (LyOutput pch, LyDur pch ~ Duration)  => ElementP pch -> Doc
oElement (Note d p)       = lyNote p d
oElement (Rest d)         = char 'r' <> optDuration d
oElement (Spacer d)       = char 's' <> optDuration d
oElement (Chord d ps)     = angles (hsep $ map lyPitch ps) <> optDuration d
oElement (GraceNotes [x]) = command "grace" <+> braces (oGrace x) where
oElement (GraceNotes xs)  = command "grace" <+> braces (lyBeam $ map oGrace xs)

oGrace :: (LyOutput e, LyDur e ~ Duration) => GraceNoteP e -> Doc
oGrace (GraceNote d p) = lyNote p d

--------------------------------------------------------------------------------
-- post process

-- Note LilyPond drops the printed repeat start if the repeat is the first
-- element (so we don't have to).



lydocRepeat :: Doc
lydocRepeat = command "repeat" <+> text "volta 2" <+> lbrace

altStart :: Doc
altStart = space <> rbrace `nextLine` command "alternative" <+> lbrace 
                            `nextLine` lbrace

altNext :: Doc
altNext = space <> rbrace `nextLine` lbrace

dblBar :: Doc 
dblBar = command "bar" <+> dquotes (text "||")

sglBar :: Doc
sglBar = text "|"


endBraces :: Int -> Doc
endBraces i | i <=0     = empty
            | otherwise = indent ((i-2)*2) rbrace `nextLine` endBraces (i-1)




--------------------------------------------------------------------------------
-- helpers


overlay :: [Doc] -> Doc
overlay = dblangles . vsep . punctuate (text " \\\\")    



note :: Pitch -> Doc 
note (Pitch l a o) = pitchLabel l a <> ove o where
    ove i | i > 0       = text $ replicate i       '\''
          | i < 0       = text $ replicate (abs i) ','
          | otherwise   = empty


pitchLabel :: PitchLetter -> Maybe Accidental -> Doc
pitchLabel l a = char (toLowerLChar l) <> maybe empty accidental a
  where 
    accidental :: Accidental -> Doc
    accidental Nat            = text "!"    -- check correctness
    accidental Sharp          = text "is"
    accidental Flat           = text "es"
    accidental DoubleSharp    = text "isis"
    accidental DoubleFlat     = text "eses"



optDuration :: Duration -> Doc
optDuration = df . lilypond where
  df []        = empty
  df [(ed,dc)] = dots dc $ either command int ed
  df _xs       = error $ "optDuration - composite todo..." 



dots :: Int -> (Doc -> Doc)
dots i | i > 0     = (<> text (replicate i '.'))
       | otherwise = id
  

lyBeam :: [Doc] -> Doc
lyBeam (x:xs) = x <> char '[' <+> hsep xs <> char ']'
lyBeam []     = empty

command :: String -> Doc
command = (char '\\' <>) . text 

comment :: String -> Doc
comment s = text "%{" <+> string s  <+> text "%}"


-- This implementation of variant key signatures is not so good...
keyCmd :: Key -> Doc
keyCmd (Key (PitchLabel l a) m) = command "key" <+> pitchLabel l a <+> mode m



timeCmd :: Meter -> Doc
timeCmd m = command "time" <+> fn m where
   fn (TimeSig n d) = integer n <> char '/' <> integer d
   fn CommonTime    = text "4/4"
   fn CutTime       = text "2/2"


mode :: Mode -> Doc
mode Major        = command "major"
mode Minor        = command "minor"
mode Lydian       = command "lydian"
mode Ionian       = command "ionian"
mode Mixolydian   = command "mixolydian"
mode Dorian       = command "dorian"
mode Aeolian      = command "aeolian"
mode Phrygian     = command "phrygian"
mode Locrian      = command "locrian"



