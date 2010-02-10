{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.AbcOutput
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Pretty print ABC.
--
--------------------------------------------------------------------------------


module Neume.AbcOutput where

import Neume.AbcDoc
import Neume.Duration
import Neume.FunctorN
import Neume.Pitch hiding ( octave )
import Neume.SyntaxStaff


import Text.PrettyPrint.Leijen

-- No annos for Abc...

type AbcGlyph = Glyph () Pitch AbcMultiplier


--------------------------------------------------------------------------------


-- | Render a phrase. This function returns a 'DPhrase' which is 
-- a list of list of Doc. To generate output, it must be 
-- post-processed. One such post-processor is 'simpleOutput'...

{-
renderPhrase :: (e -> Doc) -> Phrase e -> AbcPhrase
renderPhrase f = map (renderBarOverlay f)

renderBarOverlay :: (e -> Doc) -> Bar e -> DBar
renderBarOverlay f (Bar xs)       = [hsep $ map (renderBeam f) xs]
renderBarOverlay f (OverlayL xss) = map (hsep . map (renderBeam f)) xss

renderBeam :: (e -> Doc) -> Pulse e -> Doc
renderBeam f (Pulse e)    = f e
renderBeam f (BeamedL es) = hcat $ map f es

abcGlyph :: PDGlyphAbc -> Doc
abcGlyph (Note _ p dm t)  = note p dm <> optDoc t tie
abcGlyph (Rest dm)        = rest dm
abcGlyph (Spacer dm)      = spacer dm
abcGlyph (Chord ps dm t)  = (chordForm $ map ((note `flip` dm) . snd) ps) 
                              <> optDoc t tie
abcGlyph (GraceNotes xs)  = graceForm $ map abcGrace xs


abcGrace :: GraceNote () Pitch AbcMultiplier -> Doc
abcGrace (GraceNote _ p dm) = note p dm
-}

--------------------------------------------------------------------------------
-- Rewrite duration


-- Change Duration to a multiplier
{-

class ChangeDurationAbc t where
  changeDurationAbc :: Rational -> t Duration -> t AbcMultiplier

 
rewriteDuration :: ChangeDurationAbc t 
                => Rational 
                -> Phrase (t Duration) 
                -> Phrase (t AbcMultiplier)
rewriteDuration r bars = map (fmap (changeDurationAbc r)) bars

instance ChangeDurationAbc (Glyph anno pch) where
  changeDurationAbc r (Note a p d t)   = Note a p (abcMultiplier r d) t
  changeDurationAbc r (Rest d)         = Rest $ abcMultiplier r d
  changeDurationAbc r (Spacer d)       = Spacer $ abcMultiplier r d
  changeDurationAbc r (Chord ps d t)   = Chord ps (abcMultiplier r d) t
  changeDurationAbc r (GraceNotes xs)  = GraceNotes $ 
                                           map (changeDurationAbc r) xs

instance ChangeDurationAbc (GraceNote anno pch) where
  changeDurationAbc r (GraceNote a p d) = GraceNote a p (abcMultiplier r d)
-}

--------------------------------------------------------------------------------
-- Rewrite Pitch

-- Pitch spelling

rewritePitch :: SpellingMap
             -> StaffPhrase (Glyph anno Pitch dur) 
             -> StaffPhrase (Glyph anno Pitch dur)
rewritePitch sm = fmap (fmap3b (spell sm))

{-

fst $ stmap relpGlyph phrase p 

class ChangePitchAbc t where
  changePitchAbc :: HasPitch pch => SpellingMap -> t pch drn -> t pch drn

 
rewritePitch :: (HasPitch pch, ChangePitchAbc t)
             => SpellingMap 
             -> Phrase (t pch drn) 
             -> Phrase (t pch drn)
rewritePitch smap bars = map (fmap (changePitchAbc smap)) bars


instance ChangePitchAbc (Glyph anno) where
  changePitchAbc sm (Note a p d t)   = Note a (innerSpell sm p) d t
  changePitchAbc _  (Rest d)         = Rest d
  changePitchAbc _  (Spacer d)       = Spacer d
  changePitchAbc sm (Chord ps d t)   = Chord (map fn ps) d t
                                       where fn (a,p) = (a,innerSpell sm p)
  changePitchAbc sm (Graces xs)  = GraceNotes (map (changePitchAbc sm) xs)

innerSpell :: HasPitch p => SpellingMap -> p -> p
innerSpell sm p  = setPitch p' p where 
    p' = spell sm $ getPitch p

instance ChangePitchAbc (GraceNote anno) where
  changePitchAbc sm (GraceNote a p d) = GraceNote a (innerSpell sm p) d
-}


--------------------------------------------------------------------------------
{-

-- | Output ABC, four bars printed on each line. 
simpleOutput :: DPhrase -> Doc
simpleOutput = four . map ((<+> singleBar) . overlay)


four :: [Doc] -> Doc
four (a:b:c:d:xs) = vsep (map (<> lineCont) [a,b,c]) <$> d <$> four xs
four xs           = hsep xs

-}