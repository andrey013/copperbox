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




newtype LilyPondOutput = LilyPondOutput { getLilyPondOutput :: Doc }


generateLilyPond :: LyNote e => Key -> Meter -> PartP e -> LilyPondOutput
generateLilyPond k m a = 
    LilyPondOutput $ postProcess $ evalState (oPart a) s0 
  where
    s0       = St k m


oPart :: LyNote e => PartP e -> M (S.Seq OutputFragment)
oPart (Part as)          =
    foldlM (\a e -> (a ><) <$> oPhrase e) S.empty as


oPhrase :: LyNote e => PhraseP e -> M (S.Seq OutputFragment)
oPhrase (Phrase a)       = oMotif a
oPhrase (Repeated a)     = repeated <$> oMotif a
oPhrase (FSRepeat a x y) = fsrepeat <$> oMotif a <*> oMotif x <*> oMotif y

oMotif :: LyNote e => MotifP e -> M (S.Seq OutputFragment)
oMotif (Motif k m bs)    = motifFragment 
    <$> keyChange k keyCmd <*> meterChange m timeCmd  <*> mapM oBar bs


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

-- Note LilyPond drops the printed repeat start if the repeat is the first
-- element (so we don't have to).

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


repeat_text :: Doc
repeat_text = command "repeat" <+> text "volta 2" <+> lbrace

alt_start :: Doc
alt_start = space <> rbrace `nextLine` command "alternative" <+> lbrace 
                            `nextLine` lbrace

alt_next :: Doc
alt_next = space <> rbrace `nextLine` lbrace

dbl_bar :: Doc 
dbl_bar = command "bar" <+> dquotes (text "||")

sgl_bar :: Doc
sgl_bar = text "|"


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
    durn n d      = comment $ show n ++ "%" ++ show d

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



