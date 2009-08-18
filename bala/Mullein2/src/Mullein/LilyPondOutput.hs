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

import MonadLib.Monads

import Text.PrettyPrint.Leijen hiding ( (<$>) )

import Control.Monad
import qualified Data.Traversable as T


class LyOutput e where
  type LyDur e :: *
  lyNote  :: e -> LyDur e -> Doc
  lyPitch :: e -> Doc


class LilyPondGlyph e where
  lyGlyph :: e -> Doc

instance LilyPondGlyph (Glyph Pitch (Maybe Duration)) where
  lyGlyph = oGlyph

instance LyOutput Pitch where
  type LyDur Pitch = Maybe Duration
  lyNote p od = note p <> optDuration od
  lyPitch = note




 
-- Not so sure ...
-- might be better to return (Bar Doc)

type DOverlay   = Doc
type DBar       = [DOverlay]
type DPhrase    = [DBar]

oPhrase :: LilyPondGlyph e => Phrase e -> DPhrase
oPhrase = map oBarOverlay


oBarOverlay :: LilyPondGlyph e => Bar e -> DBar
oBarOverlay (Bar xs)       = [hsep $ map omBeam xs]
oBarOverlay (OverlayL xss) = map (hsep . map omBeam) xss


oBarOverlay' :: LilyPondGlyph e => Bar e -> Doc
oBarOverlay' (Bar xs)       = hsep (map omBeam xs)
oBarOverlay' (OverlayL xss) = overlay $ map (\xs -> hsep (map omBeam xs)) xss


omBeam :: LilyPondGlyph e => Pulse e -> Doc
omBeam (Pulse e)    = lyGlyph e
omBeam (BeamedL es) = lyBeam $ map lyGlyph es



oGlyph :: (LyOutput pch, LyDur pch ~ Maybe Duration)  => Glyph pch (Maybe Duration) -> Doc
oGlyph (Note p d)       = lyNote p d
oGlyph (Rest d)         = char 'r' <> optDuration d
oGlyph (Spacer d)       = char 's' <> optDuration d
oGlyph (Chord ps d)     = angles (hsep $ map lyPitch ps) <> optDuration d
oGlyph (GraceNotes [x]) = command "grace" <+> braces (oGrace x) where
oGlyph (GraceNotes xs)  = command "grace" <+> braces (lyBeam $ map oGrace xs)
oGlyph Tie              = char '~'

oGrace :: (LyOutput e, LyDur e ~ Maybe Duration) => GraceNote e (Maybe Duration) -> Doc
oGrace (GraceNote p d) = lyNote p d

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

doubleBar :: Doc 
doubleBar = command "bar" <+> dquotes (text "||")

singleBar :: Doc
singleBar = text "|"


endBraces :: Int -> Doc
endBraces i | i <=0     = emptyDoc
            | otherwise = indent ((i-2)*2) rbrace `nextLine` endBraces (i-1)


--------------------------------------------------------------------------------
-- rewriting


 
rewritePitch :: Pitch -> Phrase (Glyph Pitch drn) -> Phrase (Glyph Pitch drn)
rewritePitch rp bars = fst $ runState rp (mapM (T.mapM fn) bars)
  where
    fn gly = inside (changePitch `flip` gly)    

inside :: (s -> (a,s)) -> State s a
inside f = get >>= \s -> let (a,s') = f s in set s' >> return a

changePitch :: Pitch -> Glyph Pitch drn -> (Glyph Pitch drn,Pitch)  
changePitch p0 (Note p d)       = (Note (alterPitch p0 p) d, p)
changePitch p0 (Rest d)         = (Rest d, p0)
changePitch p0 (Spacer d)       = (Spacer d, p0)
changePitch p0 (Chord ps d)     = (Chord ps' d,p') 
                                  where (ps',p') = changeChordP p0 ps
changePitch p0 (GraceNotes xs)  = (GraceNotes xs',p')
                                  where (xs',p') = changeGraceP p0 xs
changePitch p0 Tie              = (Tie,p0) 



-- /Relative Pitch/ - all notes inside a grace expression change 
-- relative pitch. Only the first note of a chord changes relative 
-- pitch.


changeGraceP :: Pitch -> [GraceNote Pitch d] -> ([GraceNote Pitch d],Pitch)
changeGraceP p0 xs = anaMap fn p0 xs where
  fn (GraceNote pch d) p = Just (GraceNote (alterPitch p pch) d,pch)


changeChordP :: Pitch -> [Pitch] -> ([Pitch],Pitch)
changeChordP p0 xs = post $ anaMap fn p0 xs where
  fn pch p = Just (alterPitch p pch,pch)
  post ([],_)       = ([],p0)
  post (ps@(p:_),_) = (ps,p)

alterPitch :: Pitch -> Pitch -> Pitch
alterPitch p p'@(Pitch l oa _) = Pitch l oa (lyOctaveDist p p')
 

 
rewriteDuration :: Phrase (Glyph note Duration) 
                -> Phrase (Glyph note (Maybe Duration))
rewriteDuration bars = fst $ runState dZero (mapM (T.mapM fn) bars)
  where
    fn gly = inside (changeDuration `flip` gly)    


changeDuration :: Duration 
               -> Glyph note Duration 
               -> (Glyph note (Maybe Duration),Duration)  
changeDuration d0 (Note p d)       = (Note p (alterDuration d0 d), d)
changeDuration d0 (Rest d)         = (Rest (alterDuration d0 d), d)
changeDuration d0 (Spacer d)       = (Spacer (alterDuration d0 d), d)
changeDuration d0 (Chord ps d)     = (Chord ps (alterDuration d0 d), d)
changeDuration d0 (GraceNotes xs)  = (GraceNotes xs',d')
                                     where (xs',d') = changeGraceD d0 xs
changeDuration d0 Tie              = (Tie,d0) 

changeGraceD :: Duration 
             -> [GraceNote note Duration] 
             -> ([GraceNote note (Maybe Duration)],Duration)
changeGraceD d0 xs = anaMap fn d0 xs where
  fn (GraceNote p drn) d = Just (GraceNote p (alterDuration d drn),d)


alterDuration :: Duration -> Duration -> Maybe Duration
alterDuration d0 d | d0 == d && not (isDotted d) = Nothing
                   | otherwise                   = Just d 

    


--------------------------------------------------------------------------------
-- helpers

simpleOutput :: DPhrase -> Doc
simpleOutput = vsep . map ((<+> singleBar) . simpleOverlay) 


simpleOverlay :: [DOverlay] -> Doc
simpleOverlay []  = empty
simpleOverlay [a] = a
simpleOverlay xs  = overlay xs

overlay :: [Doc] -> Doc
overlay = dblangles . vsep . punctuate (text " \\\\") . map spacedBraces




note :: Pitch -> Doc 
note (Pitch l a o) = pitchLabel l a <> ove o where
    ove i | i > 0       = text $ replicate i       '\''
          | i < 0       = text $ replicate (abs i) ','
          | otherwise   = emptyDoc


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

comment :: String -> Doc
comment s = text "%{" <+> string s  <+> text "%}"




