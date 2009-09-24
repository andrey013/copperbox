{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS -Wall #-}
{-# OPTIONS -fno-warn-orphans #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.Mullein
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Interface to Mullein (pitch conversion, duration conversion).
--
--------------------------------------------------------------------------------

module Bala.Mullein
  ( 
  -- * Re-export internal modules
    module Bala.Mullein.Abc
  , module Bala.Mullein.LilyPond

  -- * Conversion
  , toDuration
  , toPitch

  -- * Rewriting
  , replaceRests
    
  -- * Constructors
  , mkNote
  , mkChord
  , mkRest

  -- ** LilyPond drums
  , mkDrumNote
  , mkDrumChord
  , makeDrumScore


  -- * Annotation
  , Annos(..)
  , distAnnos
  , distAnnos'

  ) where

import Bala.BeatPattern
import Bala.Duration
import Bala.Mullein.Abc
import Bala.Mullein.LilyPond
import Bala.Pitch
import Bala.Utils

import Mullein.Core ( Glyph(..), GraceNote(..) )
import qualified Mullein.Duration       as M 
import qualified Mullein.Extended       as M
import qualified Mullein.Pitch          as M
import qualified Mullein.Core           as M


import Data.Ratio

--------------------------------------------------------------------------------


instance M.HasDuration t => RationalDuration (t M.Duration) where
  rationalDuration = M.extent . M.getDuration



--------------------------------------------------------------------------------
-- Duration

toDuration :: Rational -> M.Duration
toDuration  = either restErr id . M.rationalToDuration 
  where
    restErr :: M.ConversionError -> M.Duration
    restErr e = error $ "balaDuration - cannot convert " 
                      ++ show (M.getBadRational e)
 

--------------------------------------------------------------------------------
-- Pitch

-- Mullein does not support /excessive/ accidents (e.g. triple flat)

toPitch :: Pitch -> M.Pitch
toPitch p@(Pitch l a o) 
    | a > 2 || a < (-2) = toPitch $ fromSemitones $ toSemitones p
    | otherwise         = M.Pitch (toPitchLetter l) (toAccidental a) o

toPitchLetter :: PitchLetter -> M.PitchLetter
toPitchLetter = toEnum . fromEnum

toAccidental :: Int -> Maybe M.Accidental
toAccidental n | n == (-2) = Just M.DoubleFlat
               | n == (-1) = Just M.Flat
               | n == 1    = Just M.Sharp
               | n == 2    = Just M.DoubleSharp
               | otherwise = Nothing


--------------------------------------------------------------------------------
-- Rewriting

replaceRests :: [Glyph anno pch drn] -> [Glyph anno pch drn]
replaceRests = map fn where
  fn (Rest d) = Spacer d
  fn a        = a


--------------------------------------------------------------------------------
-- 


mkNote :: Pitch -> anno -> Rational -> M.StdGlyph anno
mkNote p anno d = M.makeNote (toPitch p) anno (toDuration d)

mkChord :: [(Pitch,anno)] -> Rational -> M.StdGlyph anno
mkChord pas d = M.makeChord (map fn pas) (toDuration d) where
  fn (p,a) = (toPitch p,a)

mkRest :: M.MakeRest e => Rational -> e
mkRest = M.makeRest . toDuration

-- LilyPond drums

mkDrumNote :: M.DrumPitch -> Rational -> M.DrumGlyph
mkDrumNote p d = M.Note () p (toDuration d) False


mkDrumChord :: [M.DrumPitch] -> Rational -> M.DrumGlyph
mkDrumChord ps d = M.Chord (map f ps) (toDuration d) False
  where f a = ((),a) 




-- Amalgamate beat patterns into a score
makeDrumScore :: Rational 
              -> Rational 
              -> [M.DrumPitch] 
              -> [BeatPattern] 
              -> [M.DrumGlyph]
makeDrumScore timesig unitDuration dps patts = 
    map mkOne $ foldr (zipWith ($)) (repeat []) 
              $ map buildPitchLine 
              $ zip dps patts
  where

    buildPitchLine :: (M.DrumPitch, BeatPattern) 
                   -> [[M.DrumPitch] 
                   -> [M.DrumPitch]]
    buildPitchLine (p,bp) = map fn $ run1 timesig $ unitBeat bp 
      where 
        fn (Nb _ _) = (p:)
        fn (Rb _)   = id        

    mkOne []  = mkRest unitDuration
    mkOne [p] = mkDrumNote p unitDuration
    mkOne ps  = mkDrumChord ps unitDuration


--------------------------------------------------------------------------------
-- Annotation after creation - ideally annotations should be part 
-- of creation...




data Annos a = NA   a
             | CAs [a]
             | GAs [a]
     


distAnnos :: (ax -> ay -> az) -> [Annos ax] -> [Glyph ay p d] -> [Glyph az p d]
distAnnos upd annos glyphs = step annos glyphs where
  step (NA  a:xs)  (Note a' p d t:ys) = Note (upd a a') p d t : step xs ys

  step (CAs as:xs) (Chord pas d t:ys) = Chord pas' d t : step xs ys where
      pas' = matchZipWith chUpd as pas
      chUpd a (a',p) = (upd a a', p)

  step (GAs as:xs) (GraceNotes gs:ys) = GraceNotes gs' : step xs ys where
      gs' = matchZipWith grUpd as gs 
      grUpd a (GraceNote a' p d) = GraceNote (upd a a') p d

  step xs          (Rest d:ys)        = Rest d : step xs ys

  step xs          (Spacer d:ys)      = Spacer d : step xs ys

  -- Annos need not be empty, the might be a circular list for example...
  step _           []                 = []         

  -- But running out of annos before running out of /content/ is an error...
  step _           _                  = error $ 
                                          "distAnnos - less annos than content."
       


distAnnos' :: (ax -> ay -> az) -> [ax] -> [Glyph ay p d] -> [Glyph az p d]
distAnnos' upd annos glyphs = step annos glyphs where
  step (a:xs) (Note a' p d t:ys) = Note (upd a a') p d t : step xs ys

  step xs     (Chord pas d t:ys) = Chord pas' d t : step xs' ys where
      (pas',xs') = remZipWith chUpd pas xs
      chUpd (a',p) a = (upd a a', p)


  step xs (GraceNotes gs:ys) = GraceNotes gs' : step xs' ys where
      (gs',xs') = remZipWith grUpd gs xs
      grUpd (GraceNote a' p d) a = GraceNote (upd a a') p d

  step xs     (Rest d:ys)        = Rest d : step xs ys

  step xs     (Spacer d:ys)      = Spacer d : step xs ys
  
  step _      []                 = []

  step []     _                  = error $ 
                                     "distAnnos' - less annos than content."
