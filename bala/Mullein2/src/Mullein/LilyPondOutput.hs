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

import Control.Applicative hiding ( empty )
import Control.Monad
import qualified Data.Traversable as T


instance Applicative (State st) where
  pure = return
  (<*>) = ap

class LyOutput e where
  type LyDur e :: *
  lyNote  :: e -> LyDur e -> Doc
  lyPitch :: e -> Doc


class LilyPondGlyph e where
  lyGlyph :: e -> Doc

instance LilyPondGlyph (Glyph Pitch Duration) where
  lyGlyph = oElement  

instance LyOutput Pitch where
  type LyDur Pitch = Duration
  lyNote p od = note p <> optDuration od
  lyPitch = note




 

oBarOverlay :: LilyPondGlyph e => Bar e -> Doc
oBarOverlay (Bar xs) = hsep (map omBeam xs)
oBarOverlay _        = error "oBarOverlay TODO"


omBeam :: LilyPondGlyph e => Pulse e -> Doc
omBeam (Pulse e)    = lyGlyph e
omBeam (BeamedL es) = lyBeam $  map lyGlyph es


-- oBracket :: (LyOutput e, LyDur e ~ Duration) => OneMa (Glyph Duration e) -> Doc
-- oBracket = oneMany oElement (lyBeam . map oElement)


-- TODO check whether or not successive notes in chords and graces
-- change the relative pitch

oElement :: (LyOutput pch, LyDur pch ~ Duration)  => Glyph pch Duration -> Doc
oElement (Note p d)       = lyNote p d
oElement (Rest d)         = char 'r' <> optDuration d
oElement (Spacer d)       = char 's' <> optDuration d
oElement (Chord ps d)     = angles (hsep $ map lyPitch ps) <> optDuration d
oElement (GraceNotes [x]) = command "grace" <+> braces (oGrace x) where
oElement (GraceNotes xs)  = command "grace" <+> braces (lyBeam $ map oGrace xs)
oElement Tie              = char '~'

oGrace :: (LyOutput e, LyDur e ~ Duration) => GraceNote e Duration -> Doc
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

dblBar :: Doc 
dblBar = command "bar" <+> dquotes (text "||")

sglBar :: Doc
sglBar = text "|"


endBraces :: Int -> Doc
endBraces i | i <=0     = empty
            | otherwise = indent ((i-2)*2) rbrace `nextLine` endBraces (i-1)


--------------------------------------------------------------------------------
-- rewriting

data St = St { relativePitch :: Maybe Pitch, relativeDuration :: Duration }

lyRewrite :: Phrase (Glyph Pitch Duration) -> Phrase (Glyph Pitch (Maybe Duration))
lyRewrite bars = undefined


changeGraceP :: Pitch -> [GraceNote Pitch d] -> ([GraceNote Pitch d],Pitch)
changeGraceP p0 xs = anaMap fn p0 xs where
  fn (GraceNote pch d) p = Just (GraceNote (alterPitch p pch) d,pch)


alterPitch :: Pitch -> Pitch -> Pitch
alterPitch p p'@(Pitch l oa _) = Pitch l oa (lyOctaveDist p p')
 


runRewriteDuration :: HasDuration e => Phrase e -> Phrase e
runRewriteDuration bars = fst $ runState s0 (mapM (T.traverse rewriteDuration) bars) 
  where
    s0 = St undefined dZero

instance HasDuration St where
  getDuration            = relativeDuration
  setDuration d (St p _) = St p d

-- /Relative Duration/ - dotted durations must always be specified 
-- even if the same dotted duration appears /contiguously/. All 
-- notes inside a grace expression change relative duration, even 
-- though a grace expression is considered collectively to have 
-- /no duration/.



rewriteDuration :: (HasDuration e) => e -> State St e
rewriteDuration e = let d = getDuration e in do 
    old <- relativeDuration `fmap` get
    if d==old then return $ setDuration dZero e
              else updateCurrent d >> return e
  where
    -- | The logic here is not good - duration type needs a rethink...
    updateCurrent d | isDotted d    = setZero 
                    | otherwise     = sets_ (\s -> s {relativeDuration=d})

    setZero         = sets_ (\s -> s {relativeDuration=dZero})

-- /Relative Pitch/ - all notes inside a grace expression change 
-- relative pitch. Only the first note of a chord changes relative 
-- pitch.


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
optDuration = maybe empty df . lilypond where
  df (ed,dc) = dots dc $ either command int ed



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




