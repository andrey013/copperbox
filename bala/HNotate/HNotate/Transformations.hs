
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
import HNotate.MusicRepDatatypes
import HNotate.NoteListDatatypes
import HNotate.ProcessingTypes

import Control.Monad.Reader
import qualified Data.Foldable as F
import Data.Maybe
import Data.Sequence
import Prelude hiding (null)


--------------------------------------------------------------------------------
-- Beaming

beamNoteList :: Monad m => NoteList -> NotateT m NoteList
beamNoteList(NoteList se) = 
  return (\mp bar p -> NoteList $ fmap (beamBlock mp bar p) se)
    `ap` asks meter_pattern 
    `ap` asks bar_length 
    `ap` asks anacrusis_displacement


beamBlock :: MeterPattern -> Duration -> Duration -> Block -> Block
beamBlock mp bar_len acis blk = case blk of
    SingleBlock i bar -> SingleBlock i (block i bar)  
    PolyBlock i bars  -> PolyBlock i (fmap (block i) bars)
  where
    block i bar | i == 0 && acis /= duration_zero
                            = beamPartialBar mp acis bar
                | otherwise = beamBar mp bar
    
beamPartialBar :: MeterPattern -> Duration -> Bar -> Bar
beamPartialBar mp acis = beam (shortenMP acis mp)

beamBar :: MeterPattern -> Bar -> Bar
beamBar mp = beam mp


sumDurations :: Bar -> Duration
sumDurations = F.foldr (\e a -> a + rhythmicValue e) duration_zero


beam :: MeterPattern -> Bar -> Bar
beam mp = Bar . cata (><) empty . fmap beamUnfold . splitMeasure mp . unBar 
  where 
    unBar (Bar se) = se

-- SMst - splitMeasure state
type SMst = ([Duration],Duration, Seq Tile)

splitMeasure :: MeterPattern -> Seq Tile -> Seq (Seq Tile)
splitMeasure mp se = apo step flush (meterDivisions mp, duration_zero, se)
  where
    step :: SMst -> Maybe (Seq Tile, SMst)
    step ([],  _,_ )  = Nothing    -- run out of meter steps
    step (m:ms,d,se)  = let (d',sl,sr) = lgDurationSplit m d se in
                        if null sr then Nothing else Just (sl,(ms,d',sr))  
    
    flush :: SMst -> Seq (Seq Tile)                      
    flush (_,_,se) = singleton se

lgDurationSplit :: Duration -> Duration -> Seq Tile 
                    -> (Duration, Seq Tile, Seq Tile)
lgDurationSplit len start = 
  lgs (<=len) (\st e -> st + rhythmicValue e) start



-- | Turn a meter pattern into a list of durations
meterDivisions :: MeterPattern -> [Duration]
meterDivisions ([],s)   = []  -- is this valid?
meterDivisions (x:xs,s) = 
    scanl (\acc i -> acc + s * fromIntegral i) (s * fromIntegral x) xs

shortenMP :: Duration -> MeterPattern -> MeterPattern
shortenMP i (ds,d) = pairup $ intlist $ subn i $ drnlist ds
  where
    pairup xs = (xs,d)
    drnlist   = map ((d *) . fromIntegral)
    
    subn i []                 = []  
    subn i (d:ds) | i == d    = ds
                  | i <  d    = (d-i):ds
                  | otherwise = subn (i-d) ds 
                  
    intlist = map (round . durationToDouble . (/ d))   

    
data BeamSt = BEAM | BEAM_OFF deriving (Eq,Show)


-- beaming works nicely as an unfold as we get 'lookahead' 
-- on the input stream
beamUnfold :: Seq Tile -> Seq Tile
beamUnfold se = ana step (BEAM_OFF,se)
  where
    step (st, se) = phi (st, viewl se)
    
      -- exit the unfold
    phi (BEAM_OFF, EmptyL)        = Nothing
      -- Beam end & push back the empty seq (can't finish in the BEAM state)
    phi (BEAM,     EmptyL)        = Just  (beamEnd,   (BEAM_OFF, empty))
    
    phi (BEAM_OFF, (e :< se))   
            | eighthOrSmaller e   = case lookaheadEOS se of
                                        -- don't beam for single short notes
                                      Nothing       -> Just $ 
                                          (e,         (BEAM_OFF, se))
                                        -- start beaming & pushback e     
                                      Just (_,_) -> Just $ 
                                          (beamStart, (BEAM, e <| se))
              -- don't beam for long notes                                    
            | otherwise           = Just  (e,         (BEAM_OFF, se))
   
    phi (BEAM,    (e :< se)) 
              -- keep beaming
            | eighthOrSmaller e   = Just  (e,         (BEAM,    se))
              -- stop beaming & pushback e
            | otherwise           = Just  (beamEnd,   (BEAM_OFF, e <| se))  
    
    lookaheadEOS se = case viewl se of
                        EmptyL                        -> Nothing
                        (a :< sa) | eighthOrSmaller a -> Just (a,sa)
                                  | otherwise         -> Nothing
                
eighthOrSmaller :: Tile -> Bool
eighthOrSmaller e = rhythmicValue e <= eighth && noteOrChord e
  where
    noteOrChord (Singleton (Note _ _ _))  = True
    noteOrChord (Chord _ _ _)             = True
    noteOrChord _                         = False
    