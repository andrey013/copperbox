{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Mullein.LilyPondOutput
-- Copyright   :  (c) Stephen Tetley 2009
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
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
import Mullein.Pitch
import Mullein.Utils

import Control.Applicative hiding ( empty )
import Control.Monad.State

import Text.PrettyPrint.Leijen hiding ( (<$>) ) 
import qualified Text.PrettyPrint.Leijen as PP

data St  = St { current_key :: Key, current_meter :: Meter }

type M a = State St a


class LyPitch e where
  lyNote  :: e -> Maybe Duration -> Doc   -- for notes
  lyPitch :: e -> Doc               -- for pitches within chords, graces notes,

instance LyPitch Pitch where
  lyNote p od = note p <> optDuration od
  lyPitch p   = note p


output :: LyPitch e => Key -> Meter -> PartP e -> Doc
output k m a = evalState (oPart a) s0 where
    s0 = St k m



oPart :: LyPitch e => PartP e -> M Doc
oPart (Part as)          = vsep <$> mapM oPhrase as

oPhrase :: LyPitch e => PhraseP e -> M Doc
oPhrase (Phrase a)       = oMotif a
oPhrase (Repeated a)     = repeated <$> oMotif a
oPhrase (FSRepeat a x y) = fsrepeat <$> oMotif a <*> oMotif x <*> oMotif y

oMotif :: LyPitch e => MotifP e -> M Doc
oMotif (Motif k _ bs)    = fn <$> keyChange k <*> mapM oBar bs
  where
    fn True  xs = key k <+> (hsep $ punctuate (text " |") xs)
    fn _     xs = hsep $ punctuate (text " |") xs


oBar :: LyPitch e => BarP e -> M Doc
oBar (Bar a)             = oUnison a
oBar (Overlay a as)      = (\x xs -> overlay $ x:xs) 
                                  <$> oUnison a 
                                  <*> mapM oUnison as


oUnison :: LyPitch e => UnisonP e -> M Doc
oUnison (Unison ps tied) = (\xs -> hsep xs <> if tied then char '~'
                                                           else empty)
                                  <$> mapM oBracket ps

oBracket :: LyPitch e => BracketP e -> M Doc
oBracket (Singleton e)   = return $ oElement e
oBracket (Bracket es)    = return $ lyBeam $ map oElement es


-- TODO check whether or not successive notes in chords and graces
-- change the relative pitch

oElement :: LyPitch e => ElementP e -> Doc
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
-- helpers

-- ... COMMON ...

keyChange :: Key -> M Bool
keyChange new = do 
    old <- gets current_key 
    if (new==old) 
       then return False
       else do {modify $ \s -> s{current_key=new} ; return True } 



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
key :: Key -> Doc
key k@(Key (PitchLabel l a) m xs) 
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


repeated :: Doc -> Doc 
repeated d = command "repeat" <+> text "volta 2" <+> braces d

fsrepeat :: Doc -> Doc -> Doc -> Doc
fsrepeat a x y = 
    command "repeat" <+> text "volta 2" <+> (braces a)
        PP.<$> command "alternative" <+> braces ((braces x) <+> (braces y))
