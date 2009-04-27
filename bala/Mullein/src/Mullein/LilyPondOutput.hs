{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.LilyPondOutput
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Pretty print LilyPond
--
--------------------------------------------------------------------------------


module Mullein.LilyPondOutput where

import Mullein.CoreTypes
import Mullein.Duration
import Mullein.LabelSet
import Mullein.LilyPondNoteClass
import Mullein.OutputCommon
import Mullein.Pitch
import Mullein.Utils

import Control.Applicative hiding ( empty )
import Control.Monad.State
import Data.Foldable ( foldlM, toList )
import Data.Sequence ( (><) )
import qualified Data.Sequence as S


import Text.PrettyPrint.Leijen hiding ( (<$>) ) 
import qualified Text.PrettyPrint.Leijen as PP



type LilyPondFragment = OutputFragment BarDiv

newtype LilyPondOutput = LilyPondOutput { getLilyPondOutput :: Doc }


generateLilyPond :: LyNote e => Key -> Meter -> PartP e -> LilyPondOutput
generateLilyPond k m a = 
    LilyPondOutput $ postProcess $ evalState (oPart a) s0 
  where
    s0 = St k m



oPart :: LyNote e => PartP e -> M (S.Seq LilyPondFragment)
oPart (Part as)          =
    foldlM (\a e -> (a ><) <$> oPhrase e) S.empty as


oPhrase :: LyNote e => PhraseP e -> M (S.Seq LilyPondFragment)
oPhrase (Phrase a)       = oMotif a
oPhrase (Repeated a)     = repeated <$> oMotif a
oPhrase (FSRepeat a x y) = fsrepeat <$> oMotif a <*> oMotif x <*> oMotif y

oMotif :: LyNote e => MotifP e -> M (S.Seq LilyPondFragment)
oMotif (Motif k m bs)    = motifFragment 
    <$> keyChange k keyCmd <*> meterChange m meterCmd  <*> mapM oBar bs


oBar :: LyNote e => BarP e -> M Doc
oBar (Bar a)             = oUnison a
oBar (Overlay a as)      = (\x xs -> overlay $ x:xs) 
                                  <$> oUnison a 
                                  <*> mapM oUnison as


oUnison :: LyNote e => UnisonP e -> M Doc
oUnison (Unison ps tied) = (\xs -> hsep xs <> if tied then char '~'
                                                           else empty)
                                  <$> mapM oBracket ps

oBracket :: LyNote e => BracketP e -> M Doc
oBracket (Singleton e)   = return $ oElement e
oBracket (Bracket es)    = return $ lyBeam $ map oElement es


-- TODO check whether or not successive notes in chords and graces
-- change the relative pitch

oElement :: LyNote e => ElementP e -> Doc
oElement (Note p d)       = lyNote p (coerceDuration d)
oElement (Rest d)         = char 'r' <> oDuration d
oElement (Spacer d)       = char 's' <> oDuration d
oElement (Chord ps d)     = angles (hsep $ map lyPitch ps) <> oDuration d
oElement (GraceNotes xs)   = command "grace" <+> braces (lyBeam $ map f xs) where
                               f (p,d) = lyNote p (coerceDuration d) 


oDuration :: Duration -> Doc
oDuration = optDuration . coerceDuration

coerceDuration :: Duration -> Maybe Duration
coerceDuration d | d <= 0    = Nothing
                 | otherwise = Just d


--------------------------------------------------------------------------------
-- post process

postProcess :: S.Seq LilyPondFragment -> Doc
postProcess = fn . dropRepStart where
  fn se | S.null se           = empty
        | otherwise           = printLine $ toList se



printLine :: [LilyPondFragment] -> Doc
printLine  = step empty . intersperseBars  where
    step acc []                    = acc
    step acc (MidtuneCmd d : xs)   = step (acc `nextLine` d 
                                               `nextLine` empty) xs
    step acc (BarOutput d : xs)    = step (acc `nextLine` d) xs
    step acc (Prefix s : xs)       = step (acc <+> barDiv s) xs 
    step acc (Suffix s : xs)       = step (acc <+> barDiv s) xs


barDiv :: BarDiv -> Doc
barDiv RepStart               = repStart
barDiv RepEnd                 = repEnd
barDiv (NRep n) | n == 1      = repStart
                | otherwise   = command "alternative"
barDiv SglBar                 = char '|'
barDiv DblBar                 = command "bar" <+> dquotes (text "||")


--------------------------------------------------------------------------------
-- helpers


overlay :: [Doc] -> Doc
overlay = dblangles . vsep . punctuate (text " \\")    


note :: Pitch -> Doc 
note (Pitch l a o) = pitchLabel l a <> ove o where
    ove i | i > 0       = text $ replicate i       '\''
          | i < 0       = text $ replicate (abs i) ','
          | otherwise   = empty



-- lilypond middle c is c' 
-- HNotate middle c is c4
rescale :: Pitch -> Pitch
rescale (Pitch l a o)   = Pitch l a (o-3)

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
optDuration = maybe empty df
  where
    df 0   = empty
    df drn = let (n,d,dc) = pdElements $ augDuration drn
             in dots dc $ durn n d

    durn 4 1      = command "longa"
    durn 2 1      = command "breve"
    durn 1 i      = int i
    -- TODO - ideally we shouldn't have 'error' errors here, we should be
    -- using throwError. But that means making a lot of pure code monadic
    -- ... is there another way to do it?
    durn n d      = error $ "lyDuration failed on - " ++ show n ++ "%" ++ show d

    dots :: Int -> (Doc -> Doc)
    dots i | i > 0     = (<> text (replicate i '.'))
           | otherwise = id


lyBeam :: [Doc] -> Doc
lyBeam (x:xs) = x <> char '[' <+> hsep xs <> char ']'
lyBeam []     = empty

command :: String -> Doc
command = (char '\\' <>) . text 


-- This implementation of variant key signatures is not so good...
keyCmd :: Key -> Doc
keyCmd k@(Key (PitchLabel l a) m xs) 
    | null xs   = command "key" <+> pitchLabel l a <+> mode m
    | otherwise = command "set" <+> text "Staff.keySignature" 
                                <+> text "= #`" <> parens scm 

  where
    scm           = vsep $ map f $ trebleKeyMarks k
    f (o,s,acdt)  = parens ((parens $ int o <> PP.dot <> int s) 
                            <+> PP.dot <+> g acdt)
    g Nat         = text ",NAT"
    g Sharp       = text ",SHARP"
    g DoubleSharp = text ",DOUBLE-SHARP"
    g Flat        = text ",FLAT"
    g DoubleFlat  = text ",DOUBLE-FLAT"


meterCmd :: Meter -> Doc
meterCmd _ = text "\\meter - TODO"


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



repStart :: Doc
repStart = command "repeat" <+> text "volta 2" <+> lbrace

repEnd :: Doc
repEnd = rbrace

{-

fsrepeat :: Doc -> Doc -> Doc -> Doc
fsrepeat a x y = 
    command "repeat" <+> text "volta 2" <+> (braces a)
        PP.<$> command "alternative" <+> braces ((braces x) <+> (braces y))
-}
