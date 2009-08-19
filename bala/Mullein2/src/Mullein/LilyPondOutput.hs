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


module Mullein.LilyPondOutput 
  (
  -- * Glyph class
    LilyPondGlyph(..)

  -- * Render    
  , renderPhrase

  -- * Rewriting
  , ChangeDurationLR(..)
  , rewriteDuration
  
  , ChangePitchLR(..)
  , rewritePitch

  -- * Post-process and pretty print
  , simpleOutput 

  , note
  , pitch
  , rest
  , spacer
  , chordForm
  , graceForm
  , tie
  , optDuration

  , doubleBar
  , singleBar 

  -- * Alternative types
  , DrumPitch(..)
  , DrumGlyph
   
  ) where

import Mullein.Core
import Mullein.Duration
import Mullein.Pitch
import Mullein.Utils

import MonadLib.Monads

import Text.PrettyPrint.Leijen

import Control.Monad
import qualified Data.Traversable as T




--------------------------------------------------------------------------------
-- Classes

-- | To be renderable as LilyPond, glyphs must implement this class.
class LilyPondGlyph e where
  lyGlyph :: e -> Doc

instance LilyPondGlyph (Glyph Pitch (Maybe Duration)) where
  lyGlyph = oGlyph pitch


instance LilyPondGlyph (Glyph DrumPitch (Maybe Duration)) where
  lyGlyph = oGlyph (\(DrumPitch short _) -> text short)


--------------------------------------------------------------------------------
-- Render

-- | Render a phrase. This function returns a 'DPhrase' which is 
-- a list of list of Doc. To generate output, it must be 
-- post-processed. One such post-processor is 'simpleOutput'...
renderPhrase :: LilyPondGlyph e => Phrase e -> DPhrase
renderPhrase = map oBarOverlay


oBarOverlay :: LilyPondGlyph e => Bar e -> DBar
oBarOverlay (Bar xs)       = [hsep $ map omBeam xs]
oBarOverlay (OverlayL xss) = map (hsep . map omBeam) xss


omBeam :: LilyPondGlyph e => Pulse e -> Doc
omBeam (Pulse e)    = lyGlyph e
omBeam (BeamedL es) = lyBeam $ map lyGlyph es



oGlyph :: (pch -> Doc) -> Glyph pch (Maybe Duration) -> Doc
oGlyph f (Note p d)       = f p <> optDuration d
oGlyph _ (Rest d)         = rest d
oGlyph _ (Spacer d)       = spacer d
oGlyph f (Chord ps d)     = chordForm (map f ps) d
oGlyph f (GraceNotes xs)  = graceForm (map (oGrace f) xs)
oGlyph _ Tie              = tie

oGrace :: (pch -> Doc) -> GraceNote pch (Maybe Duration) -> Doc
oGrace f (GraceNote p d) = f p <> optDuration d




--------------------------------------------------------------------------------
-- rewriting

-- Duration


-- The duration change for LilyPond is stateful (it is 
-- relative to the previous duration), so we have to pass the 
-- state around c.f. unfoldr or the State monad. 
-- Having a simple ChangeDuration class is not useful here. Such 
-- a class might be something like:
--
-- @ class ChangeDuration t where
-- @   changeDuration :: d -> t Duration -> t d
--

-- mnemonic LR - (L)ilypond (R)elative

class ChangeDurationLR t where
  changeDurationLR :: Duration -> t Duration -> (t (Maybe Duration), Duration)

 
rewriteDuration :: ChangeDurationLR t
                => Phrase (t Duration) -> Phrase (t (Maybe Duration))
rewriteDuration bars = fst $ runState dZero (mapM (T.mapM fn) bars)
  where
    fn gly = inside $ (changeDurationLR `flip`  gly)    


instance ChangeDurationLR (Glyph pch) where
  changeDurationLR d0 (Note p d)       = (Note p (alterDuration d0 d), d)
  changeDurationLR d0 (Rest d)         = (Rest (alterDuration d0 d), d)
  changeDurationLR d0 (Spacer d)       = (Spacer (alterDuration d0 d), d)
  changeDurationLR d0 (Chord ps d)     = (Chord ps (alterDuration d0 d), d)
  changeDurationLR d0 (GraceNotes xs)  = (GraceNotes xs',d')
                                          where (xs',d') = changeGraceD d0 xs
  changeDurationLR d0 Tie              = (Tie,d0) 



changeGraceD :: Duration 
             -> [GraceNote note Duration] 
             -> ([GraceNote note (Maybe Duration)],Duration)
changeGraceD d0 xs = anaMap fn d0 xs where
  fn (GraceNote p drn) d = Just (GraceNote p (alterDuration d drn),d)


alterDuration :: Duration -> Duration -> Maybe Duration
alterDuration d0 d | d0 == d && not (isDotted d) = Nothing
                   | otherwise                   = Just d 

    

-- Relative Pitch

class ChangePitchLR t where
  changePitchLR :: HasPitch pch => Pitch -> t pch drn -> (t pch drn, Pitch)

 
rewritePitch :: (ChangePitchLR t, HasPitch pch) 
             => Pitch -> Phrase (t pch drn) -> Phrase (t pch drn)
rewritePitch rp bars = fst $ runState rp (mapM (T.mapM fn) bars)
  where
    fn gly = inside (changePitchLR `flip` gly)    

inside :: (s -> (a,s)) -> State s a
inside f = get >>= \s -> let (a,s') = f s in set s' >> return a

instance ChangePitchLR Glyph where
  changePitchLR p0 (Note p d)       = (Note p' d, getPitch p)
                                      where p' = alterPitch p0 p
  changePitchLR p0 (Rest d)         = (Rest d, p0)
  changePitchLR p0 (Spacer d)       = (Spacer d, p0)
  changePitchLR p0 (Chord ps d)     = (Chord ps' d,p') 
                                       where (ps',p') = changeChordP p0 ps
  changePitchLR p0 (GraceNotes xs)  = (GraceNotes xs',p')
                                       where (xs',p') = changeGraceP p0 xs
  changePitchLR p0 Tie              = (Tie,p0) 



-- /Relative Pitch/ - all notes inside a grace expression change 
-- relative pitch. Only the first note of a chord changes relative 
-- pitch.


changeGraceP :: HasPitch pch 
             => Pitch -> [GraceNote pch d] -> ([GraceNote pch d],Pitch)
changeGraceP p0 xs = anaMap fn p0 xs where
  fn (GraceNote pch d) p = Just (GraceNote (alterPitch p pch) d, getPitch pch)


changeChordP :: HasPitch pch => Pitch -> [pch] -> ([pch],Pitch)
changeChordP p0 xs = post $ anaMap fn p0 xs where
  fn pch st = Just (alterPitch st pch, getPitch pch)
  post ([],_)       = ([],p0)
  post (ps@(p:_),_) = (ps,getPitch p)

alterPitch :: HasPitch pch => Pitch -> pch -> pch
alterPitch p0 pch = setPitch p1 pch
  where
    p  = getPitch pch
    p1 = changeOctave p0 p 
    

changeOctave :: Pitch -> Pitch -> Pitch
changeOctave p p' = modifyOctave (lyOctaveDist p p') p'
 



--------------------------------------------------------------------------------
-- helpers



simpleOutput :: DPhrase -> Doc
simpleOutput = vsep . map ((<+> singleBar) . simpleOverlay)


simpleOverlay :: [Doc] -> Doc
simpleOverlay []  = empty
simpleOverlay [a] = a
simpleOverlay xs  = overlay xs

overlay :: [Doc] -> Doc
overlay = dblangles . vsep . punctuate (text " \\\\") . map spacedBraces


note :: Pitch -> Maybe Duration -> Doc
note p md = pitch p <> optDuration md

pitch :: Pitch -> Doc 
pitch (Pitch l a o) = pitchLabel l a <> ove o where
    ove i | i > 0       = text $ replicate i       '\''
          | i < 0       = text $ replicate (abs i) ','
          | otherwise   = emptyDoc

rest :: Maybe Duration -> Doc
rest md = char 'r' <> optDuration md

spacer :: Maybe Duration -> Doc
spacer md = char 's' <> optDuration md

chordForm :: [Doc] -> Maybe Duration -> Doc
chordForm xs md = angles (hsep xs) <> optDuration md

graceForm :: [Doc] -> Doc
graceForm [x] = command "grace" <+> braces x where
graceForm xs  = command "grace" <+> braces (lyBeam xs)


tie :: Doc
tie = char '~'

pitchLabel :: PitchLetter -> Maybe Accidental -> Doc
pitchLabel l a = char (toLowerLChar l) <> maybe emptyDoc accidental a
  where 
    accidental :: Accidental -> Doc
    accidental Nat            = text "!"    -- check correctness
    accidental Sharp          = text "is"
    accidental Flat           = text "es"
    accidental DoubleSharp    = text "isis"
    accidental DoubleFlat     = text "eses"



optDuration :: Maybe Duration -> Doc
optDuration Nothing  = emptyDoc
optDuration (Just d) = maybe emptyDoc df $ lyRepresentation d where
  df (LyCmd ss,dc) = dots dc $ command ss 
  df (LyNum i,dc)  = dots dc $ int i



dots :: Int -> (Doc -> Doc)
dots i | i > 0     = (<> text (replicate i '.'))
       | otherwise = id
  
-- | Beams - first element printed outside the square brackets, e.g.:
-- @ c [e g] @
--  
lyBeam :: [Doc] -> Doc
lyBeam (x:xs) = x <> char '[' <+> hsep xs <> char ']'
lyBeam []     = emptyDoc

command :: String -> Doc
command = (char '\\' <>) . text 

{-

-- TODO - Will it be better to have a separate module for LilyPond
-- pretty printers?


comment :: String -> Doc
comment s = text "%{" <+> string s  <+> text "%}"

direction :: Direction -> Doc 
direction Above  = char '^' 
direction Below  = char '_'
direction Center = char '-'


-- Note LilyPond drops the printed repeat start if the repeat is the first
-- element (so we don't have to).


lydocRepeat :: Doc
lydocRepeat = command "repeat" <+> text "volta 2" <+> lbrace

altStart :: Doc
altStart = space <> rbrace `nextLine` command "alternative" <+> lbrace 
                           `nextLine` lbrace

altNext :: Doc
altNext = space <> rbrace `nextLine` lbrace
-}

doubleBar :: Doc 
doubleBar = command "bar" <+> dquotes (text "||")

singleBar :: Doc
singleBar = text "|"


{-

endBraces :: Int -> Doc
endBraces i | i <=0     = emptyDoc
            | otherwise = indent ((i-2)*2) rbrace `nextLine` endBraces (i-1)

-}



--------------------------------------------------------------------------------
-- Alternative types 

-- Maybe these should be in Mullein.Extended ?

-- Drums

data DrumPitch = DrumPitch { 
      drum_long_name   :: String, 
      drum_short_name  :: String 
    }
  deriving (Eq,Show)


type DrumGlyph = Glyph DrumPitch Duration


instance MakeRest DrumGlyph where
  makeRest drn = Rest drn

{-

-- Spacer marks are /syntax/ are the glyph level.

data SpacerMark drn = SpacerMark Direction Doc drn
  deriving (Show)

data Direction = Above | Below | Center
  deriving (Eq,Show)

-}
