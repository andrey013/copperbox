
--------------------------------------------------------------------------------
-- |
-- Module      :  HNotate.Transformations
-- Copyright   :  (c) Stephen Tetley 2008
-- License     :  BSD-style (as per the Haskell Hierarchical Libraries)
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  to be determined.
--
-- Transformations on NoteLists that don't necessarily preverse shape
-- (and so can't be done with Data.Traversable).
--
--------------------------------------------------------------------------------

module HNotate.Transformations where

import HNotate.CommonUtils
import HNotate.Duration
import HNotate.Env
import HNotate.Fits
import HNotate.MusicRepDatatypes
import HNotate.NoteListDatatypes
import HNotate.ProcessingTypes
import HNotate.SequenceUtils

import Control.Monad.Reader
import qualified Data.Foldable as F
import Data.Sequence
import Numeric (fromRat)
import Prelude hiding (null, length)


--------------------------------------------------------------------------------
-- Beaming

beamNoteList :: Monad m => NoteList -> NotateT m NoteList
beamNoteList(NoteList se) = 
  return beamStep `ap` asks meter_pattern 
                  `ap` asks bar_length 
                  `ap` asks anacrusis_displacement
  where
    beamStep mp bar p = 
      NoteList $ sziplWith (\ blk i -> (beamBlock i mp bar p blk)) se [0..]



beamBlock :: Int -> MeterPattern -> Duration -> Duration -> Block -> Block
beamBlock i mp barlen asis blk = case blk of
    SingleBlock bar -> SingleBlock $ fn i bar
    OverlayBlock bars -> OverlayBlock $ fmap (fn i) bars
  where
    fn i bar 
        | i == 0 && asis /= duration_zero = beam asis barlen mp bar
        | otherwise                       = beam 0    barlen mp bar


beam :: Duration -> Duration -> MeterPattern -> Bar -> Bar
beam asis barlen mp = 
    Bar . cata (><) empty . fmap beamGroup 
                          . splitMeasure (reduceMeterPattern asis barlen mp) 
                          . unBar 
  where 
    unBar (Bar se) = se


reduceMeterPattern :: Duration -> Duration -> MeterPattern -> MeterPattern
reduceMeterPattern asis barlen (patts,d) 
    | asis == 0 = (patts,d)
    | otherwise = (advance remaining patts,d)  
  where    
    remaining :: Int
    remaining = floor $ fromRat $ rlen / mplen
    
    mplen :: Duration
    mplen = meterPatternLength (patts,d)
    
    rlen :: Duration
    rlen = barlen - asis 


splitMeasure :: MeterPattern -> Seq Grouping -> Seq (Seq Grouping)
splitMeasure (mp,d) se = regiment se (fmap (\m -> d * makeDuration m 1) mp)

beamGroup :: Seq Grouping -> Seq Grouping
beamGroup = F.foldr fn empty . joinLTeighth where
  fn e a | length e > 1   = (beamStartSgl <| (e |> beamEndSgl)) >< a
         | otherwise      = e >< a
  joinLTeighth = sgroupBy (\a b -> eighthOrSmallerAndNote a 
                                   && eighthOrSmallerAndNote b)

-- only want to be chords and notes, not rest, tuplets etc  
eighthOrSmallerAndNote :: Grouping -> Bool
eighthOrSmallerAndNote gp = rhythmicValue gp <= eighth && noteOrChord gp
  where
    noteOrChord (Singleton (Note _ _ _))  = True
    noteOrChord (Chord _ _ _)             = True
    noteOrChord _                         = False


   