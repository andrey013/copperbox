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
  lydocNote  :: e -> LyDur e -> Doc
  lydocPitch :: e -> Doc


class LilyPondGlyph e where
  lyGlyph :: e -> Doc

instance LilyPondGlyph (ElementP ScNote) where
  lyGlyph = oElement  

instance LyOutput Pitch where
  type LyDur Pitch = Maybe Duration
  lydocNote p od = note p <> optDuration od
  lydocPitch = note

instance LyOutput ScNote where
  type LyDur ScNote = Maybe Duration
  lydocNote (ScNote p _) od = note p <> optDuration od
  lydocPitch (ScNote p _) = note p



newtype LilyPondOutput = LilyPondOutput { getLilyPondOutput :: Doc }

{-

generateLilyPond :: LyNote e => Key -> Meter -> PartP e -> LilyPondOutput
generateLilyPond k m a = 
    LilyPondOutput $ postProcess $ evalState (oPart a) s0 
  where
    s0       = OutputSt k m


oPart :: LyNote e => PartP e -> OutputM (S.Seq OutputFragment)
oPart (Part as)          =
    foldlM (\a e -> (a ><) <$> oPhrase e) S.empty as


oPhrase :: LyNote e => PhraseP e -> OutputM (S.Seq OutputFragment)
oPhrase (Phrase a)       = oMotif a
oPhrase (Repeated a)     = repeated <$> oMotif a
oPhrase (FSRepeat a x y) = fsrepeat <$> oMotif a <*> oMotif x <*> oMotif y

oMotif :: LyNote e => MotifP e -> OutputM (S.Seq OutputFragment)
oMotif (Motif k m bs)    = motifFragment 
    <$> keyChange k keyCmd <*> meterChange m timeCmd  <*> mapM oBar bs


oBar :: LyNote e => BarP e -> OutputM Doc
oBar (Bar a)             = oUnison a
oBar (Overlay a as)      = (\x xs -> overlay $ x:xs) 
                                  <$> oUnison a 
                                  <*> mapM oUnison as


oUnison :: LyNote e => UnisonP e -> OutputM Doc
oUnison (Unison ps tied) = (\xs -> hsep xs <> if tied then char '~'
                                                           else empty)
                                  <$> mapM oBracket ps

-}

-- oBar 

oBarOverlay :: LilyPondGlyph e => (Bool,[OneMany e]) -> Doc
oBarOverlay (tied,xs) = hsep (map omBeam xs) <> if tied then char '~' else empty


omBeam :: LilyPondGlyph e => OneMany e -> Doc
omBeam = oneMany lyGlyph (lyBeam . map lyGlyph) 

oBracket :: (LyOutput e, LyDur e ~ Maybe Duration) => OneMany (ElementP e) -> Doc
oBracket = oneMany oElement (lyBeam . map oElement)


-- TODO check whether or not successive notes in chords and graces
-- change the relative pitch

oElement :: (LyOutput pch, LyDur pch ~ Maybe Duration)  => ElementP pch -> Doc
oElement (Note d p)       = lydocNote p (coerceDuration d)
oElement (Rest d)         = char 'r' <> oDuration d
oElement (Spacer d)       = char 's' <> oDuration d
oElement (Chord d ps)     = angles (hsep $ map lydocPitch ps) <> oDuration d
oElement (GraceNotes [x]) = command "grace" <+> braces (oGrace x) where
oElement (GraceNotes xs)  = command "grace" <+> braces (lyBeam $ map oGrace xs)

oGrace :: (LyOutput e, LyDur e ~ Maybe Duration) => (e,Duration) -> Doc
oGrace (p,d) = lydocNote p (coerceDuration d)

oDuration :: Duration -> Doc
oDuration = optDuration . coerceDuration

coerceDuration :: Duration -> Maybe Duration
coerceDuration d | isZero d  = Nothing
                 | otherwise = Just d


--------------------------------------------------------------------------------
-- post process

-- Note LilyPond drops the printed repeat start if the repeat is the first
-- element (so we don't have to).

{-

postProcess :: S.Seq OutputFragment -> Doc
postProcess = fn . intersperseBars . toList . addDblEnd where
  fn []           = empty
  fn xs           = printFrags xs


printFrags :: [OutputFragment] -> Doc
printFrags  = genUnfold2 phi nextLine empty 0 where
    phi _ []                    = Nothing
    phi i (MidtuneCmd d : xs)   = Just (indent (i*2) d,i,xs)
    
    -- lookahead
    phi i (BarOutput d : SglBar : xs)    
                                = Just (indent (i*2) d <+> sgl_bar,i,xs)
    phi i (BarOutput d : DblBar : xs) 
                                = Just (indent (i*2) d <+> dbl_bar,i,xs)
   
    phi i (BarOutput d : xs)    = Just (indent (i*2) d,i,xs)

    phi i (RepStart : xs)       = Just (indent (i*2) repeat_text, 1, xs)
    phi i (RepEnding n : xs) 
          | n == 1              = Just (indent (i*2) alt_start, i+1, xs)
          | otherwise           = Just (indent (i*2) alt_next, i, xs)
    phi i (RepEnd : xs)         = Just (endBraces i, 0, xs)
    phi i (SglBar : xs)         = Just (indent (i*2) sgl_bar, i, xs)
    phi i (DblBar : xs)         = Just (indent (i*2) dbl_bar, i, xs)   

-}



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


pitchLabel :: PitchLetter -> Accidental -> Doc
pitchLabel l a = char (toLowerLChar l) <> accidental a
  where 
    accidental :: Accidental -> Doc
    accidental Nat            = empty
    accidental Sharp          = text "is"
    accidental Flat           = text "es"
    accidental DoubleSharp    = text "isis"
    accidental DoubleFlat     = text "eses"



optDuration :: Maybe Duration -> Doc
optDuration = maybe empty (df . lilypond) where
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



