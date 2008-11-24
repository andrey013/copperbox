{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Bala.MusicRep.Pulse
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Develop rhythmic motifs...
--
--------------------------------------------------------------------------------

-- TODO - need to be able to overlay a motif with a longer pulse onto one
-- with a shorter one 

module Bala.MusicRep.Pulse where

import Bala.Base.BaseExtra
import Bala.Base.Duration
import Bala.Base.Metrical
import Bala.Base.Pitch
import Bala.Base.Structural
import Bala.Base.FocusedShapeContents

import HNotate.Fits (segment, sumMeasure)

import qualified Data.Foldable as F
import Data.Sequence

import Text.PrettyPrint.HughesPJ hiding (empty)


-- Maybe not -- there's not obvious fits instance for Clave
type ClaveSection   = SectionF Clave
type ClavePhrase    = PhraseF Clave
type ClaveMotif     = MotifF Clave

type RhythmicSection   = SectionF RhythmicEvent
type RhythmicPhrase    = PhraseF RhythmicEvent
type RhythmicMotif     = MotifF RhythmicEvent


-- don't need this cud is shape preserving so it can be done with fmap


claveUnitDuration :: Functor t => Duration -> t Clave -> t RhythmicEvent
claveUnitDuration d t = fmap fn t where
    fn ClaveOn  = Sounds d
    fn ClaveOff = Rests d

unitNote :: Functor t => Pitch -> t RhythmicEvent -> t Event
unitNote p t = fmap fn t where
    fn (Sounds d) = note p d
    fn (Rests d)  = rest d   
     
{-

class ClaveUnitDuration a b | a -> b where 
    claveUnitDuration :: Duration -> a -> b

instance ClaveUnitDuration ClaveSection RhythmicSection where
  claveUnitDuration d (Section tm se)   = 
      Section tm (fmap (claveUnitDuration d) se)

instance ClaveUnitDuration ClavePhrase RhythmicPhrase where
  claveUnitDuration d (Single mo)       = Single $ claveUnitDuration d mo 
  claveUnitDuration d (Overlay mo smo)  = 
        Overlay (claveUnitDuration d mo) (fmap (claveUnitDuration d) smo) 
      
      
instance ClaveUnitDuration ClaveMotif RhythmicMotif where
  claveUnitDuration d (Motif se) = Motif $ fmap fn se 
    where
      fn ClaveOn  = Sounds d
      fn ClaveOff = Rests d
-}    


-- eg. [X X . x X . x .] - X hi x lo
   
data TriClave = Hi | Lo | TriOff  
  deriving (Eq,Enum,Show)

{-

-- Type level durations
data Whole
data Half
data Quarter
data Eighth
data Sixteenth

newtype RhythmicSection a = RSection { getRSection :: SectionF RhythmicEvent }
newtype RhythmicPhrase  a = RPhrase  { getRPhrase  :: PhraseF  RhythmicEvent }
newtype RhythmicMotif   a = RMotif   { getRMotif   :: MotifF   RhythmicEvent }



wrapOverlay :: [RhythmicMotif a] -> RhythmicPhrase a
wrapOverlay = RPhrase . overlay . fmap getRMotif

tap :: Integral a => Duration -> a -> RhythmicEvent
tap d i = Sounds $ d * makeDuration i 1

off :: Integral a => Duration -> a -> RhythmicEvent
off d i = Rests $ d * makeDuration i 1

  

class BuildRhythmicMotif a where
  build :: [Duration -> Int -> RhythmicEvent] -> [Int] -> RhythmicMotif a

-- don't export build' (when you decide on the export list...)
build' :: Duration -> [Duration -> Int -> RhythmicEvent] -> [Int] -> RhythmicMotif a
build' d fs xs = RMotif $ motifl $ zipWith fn xs (cycle fs)
  where fn x f = f d x

instance BuildRhythmicMotif Whole where
  build = build' whole
  
instance BuildRhythmicMotif Half where
  build = build' half
  
instance BuildRhythmicMotif Quarter where
  build = build' quarter

instance BuildRhythmicMotif Eighth where
  build = build' eighth 
  
instance BuildRhythmicMotif Sixteenth where
  build = build' sixteenth 
  


durationFocus :: RhythmicValue a => Focus a Duration
durationFocus = 
    FN { focus = fc, extract = rhythmicValue, putback = modifyDuration }
  where    
    fc = (/= duration_zero) . rhythmicValue

-- This makes an onset clave pattern - i.e. an On clave event indicates just 
-- the onset of an event not its duration.
-- Note the fundep:
--  RhythmicMotif produces MotifF Clave
--  RhythmicPhrase produces PhraseF Clave
 
class ClavePattern c t a | c -> t where clavePattern :: c a -> t Clave

instance ClavePattern RhythmicMotif MotifF Whole where
  clavePattern = motifClavePattern whole 
  
instance ClavePattern RhythmicMotif MotifF Half where
  clavePattern = motifClavePattern half 
  
instance ClavePattern RhythmicMotif MotifF Quarter where
  clavePattern = motifClavePattern quarter 

instance ClavePattern RhythmicMotif MotifF Eighth where
  clavePattern = motifClavePattern eighth 
  
instance ClavePattern RhythmicMotif MotifF Sixteenth where
  clavePattern = motifClavePattern sixteenth 

instance ClavePattern RhythmicPhrase PhraseF Whole where
  clavePattern = phraseClavePattern whole 
  
instance ClavePattern RhythmicPhrase PhraseF Half where
  clavePattern = phraseClavePattern half 
  
instance ClavePattern RhythmicPhrase PhraseF Quarter where
  clavePattern = phraseClavePattern quarter 
  
instance ClavePattern RhythmicPhrase PhraseF Eighth where
  clavePattern = phraseClavePattern eighth 

instance ClavePattern RhythmicPhrase PhraseF Sixteenth where
  clavePattern = phraseClavePattern sixteenth 

instance ClavePattern RhythmicSection SectionF Whole where
  clavePattern = sectionClavePattern whole
  
instance ClavePattern RhythmicSection SectionF Half where
  clavePattern = sectionClavePattern half
   
instance ClavePattern RhythmicSection SectionF Quarter where
  clavePattern = sectionClavePattern quarter 

instance ClavePattern RhythmicSection SectionF Eighth where
  clavePattern = sectionClavePattern eighth 
  
instance ClavePattern RhythmicSection SectionF Sixteenth where
  clavePattern = sectionClavePattern sixteenth 
  
  
  
-- don't export...
motifClavePattern :: Duration -> RhythmicMotif a -> MotifF Clave
motifClavePattern base (RMotif (Motif se)) = Motif $ F.foldl fn empty se where
  fn :: Seq Clave -> RhythmicEvent -> Seq Clave
  fn a (Sounds d)  = (a |> ClaveOn)  `padless1` (multiple d base)
  fn a (Rests d)   = (a |> ClaveOff) `padless1` (multiple d base)
  
  padless1 a (n+1) | n <= 0     = a
                   | otherwise  = a >< sreplicate n ClaveOff  
   
  multiple :: Duration -> Duration -> Int
  multiple d base = let (a,r) = d `divModR` base in 
      if r == 0 then fromIntegral a else error msg
    where  
      msg = "multiple used on a duration that is not an "
         ++ "exact multiple of the base" 

-- don't export...
phraseClavePattern :: Duration -> RhythmicPhrase a -> PhraseF Clave
phraseClavePattern base (RPhrase (Single mo))       = 
    Single $ motifClavePattern base (RMotif mo)
    
phraseClavePattern base (RPhrase (Overlay mo smo))  = 
    Overlay (fn mo) (fmap fn smo)
  where    
    fn = motifClavePattern base . RMotif
    
sectionClavePattern :: Duration -> RhythmicSection a -> SectionF Clave
sectionClavePattern base (RSection (Section tm se)) = 
  Section tm $ fmap (phraseClavePattern base . RPhrase) se

-}


-- n and k are flipped from bjorklund...
    
euclidRhythm :: Int -> Int -> MotifF Clave
euclidRhythm k n = Motif $ fromList $ bjorklund n k

readClave :: Eq a => a -> [a] -> MotifF Clave
readClave a = Motif . fromList . step where 
    step (x:xs) | x == a    = ClaveOn  : step xs
                |otherwise  = ClaveOff : step xs
    step []                 = []




