{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS -Wall #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  Neume.Core.Bracket
-- Copyright   :  (c) Stephen Tetley 2010
-- License     :  BSD3
--
-- Maintainer  :  Stephen Tetley <stephen.tetley@gmail.com>
-- Stability   :  highly unstable
-- Portability :  GHC
--
-- Bar splitting and beam grouping.
--
--------------------------------------------------------------------------------

module Neume.Core.Bracket 
  ( 
    phrase
  , phraseAna

  ) where

import Neume.Core.Duration
import Neume.Core.Metrical
import Neume.Core.NoteList
import Neume.Core.SyntaxStaff
import Neume.Core.Utils



import qualified Data.Foldable as F 
import Data.Sequence 
import Prelude hiding ( null )


   


-- | A Metric unit represents a division of a NoteList according
-- to the sizes of a 'MeterPattern'.
--
--

type MetricUnit   a = Seq (PletTree a) 
type SegmentState a = (DurationMeasure, PletForest a)
type BarMU        a = Seq (MetricUnit a)

mbSnoc :: Seq a -> Maybe a -> Seq a
mbSnoc se Nothing  = se
mbSnoc se (Just a) = se |> a



--------------------------------------------------------------------------------
-- 

phrase :: (Measurement a ~ DurationMeasure, NumMeasured a, BeamExtremity a) 
       => MeterPattern -> NoteList a -> StaffPhrase a
phrase mp (NoteList name notes) = 
    StaffPhrase name $ fmap beamBar $ splitToBars mp notes


phraseAna :: (Measurement a ~ DurationMeasure, NumMeasured a, BeamExtremity a) 
       => DurationMeasure -> MeterPattern -> NoteList a -> StaffPhrase a
phraseAna ana mp (NoteList name notes) = 
    StaffPhrase name $ fmap beamBar $ anaSplitToBars ana mp notes
 


--------------------------------------------------------------------------------
-- new version with snoc-able accumulator...



splitToBars :: (Measurement a ~ DurationMeasure, NumMeasured a) 
            => MeterPattern -> PletForest a -> Seq (BarMU a)
splitToBars mp notes = workerSplit mp empty (0,notes) where

anaSplitToBars :: (Measurement a ~ DurationMeasure, NumMeasured a) 
               => DurationMeasure -> MeterPattern -> PletForest a -> Seq (BarMU a)
anaSplitToBars ana mp notes = workerSplit mp (singleton bar0) state0
  where
    (bar0,state0) = firstAna ana mp notes


workerSplit :: (Measurement a ~ DurationMeasure, NumMeasured a) 
            => MeterPattern -> Seq (BarMU a) -> (SegmentState a) -> Seq (BarMU a)
workerSplit mp = step where
  step acc (_,[]) = acc
  step acc st     = let (bar,st') = nextBar mp st in step (acc |> bar) st'
  


firstAna :: (Measurement a ~ DurationMeasure, NumMeasured a) 
         => DurationMeasure -> MeterPattern -> PletForest a 
         -> (BarMU a, SegmentState a)
firstAna ana mp0 notes = nextBar (anacrusis ana mp0) (0,notes)


nextBar ::  (Measurement a ~ DurationMeasure, NumMeasured a) 
        => MeterPattern -> SegmentState a -> (BarMU a, SegmentState a)
nextBar mp = step empty mp where
  step acc []     st = (acc,st)
  step acc (d:ds) st = step (acc `mbSnoc` a) ds st'
                       where (a,st') = nextMUnit d st


nextMUnit :: (Measurement a ~ DurationMeasure, NumMeasured a)  
          => DurationMeasure
          -> SegmentState a
          -> (Maybe (MetricUnit a), SegmentState a)
nextMUnit d (borrow,notes) | borrow >= d = (Nothing, (borrow-d,notes))
                           | otherwise   = fmap2a Just $ nextMUnit1 (d-borrow) notes

-- Potentially consume more than the required duration (as notes 
-- cannot be split).

nextMUnit1 :: (Measurement a ~ DurationMeasure, NumMeasured a)
           => DurationMeasure
           -> PletForest a
           -> (MetricUnit a, SegmentState a)
nextMUnit1 = step empty where
  step acc d xs       | d <= 0 = (acc,(abs d,xs))
  step acc d []                = (acc,(abs d,[]))
  step acc d (x:xs)            = body acc d x xs

  body acc d (S a)  xs  = step (acc |> S a)    (d - nmeasure a) xs
  body acc d p_tree xs  = step (acc |> p_tree) (d - pletMeasure p_tree) xs


--------------------------------------------------------------------------------

-- Reconstitute the beam groups

beamBar :: (Measurement a ~ DurationMeasure, NumMeasured a, BeamExtremity a) 
        => BarMU a -> Seq (CExpr a)
beamBar = F.foldl (><) empty . fmap beamMU

beamMU :: (Measurement a ~ DurationMeasure, NumMeasured a, BeamExtremity a) 
       => MetricUnit a -> Seq (CExpr a)
beamMU = step1 . forward where
  step1 (left,rest) | null rest = left
                    | otherwise = left >< step2 (backward rest)

  step2 (middle,right)          = mkBeamed middle >< right


forward :: (Measurement a ~ DurationMeasure, NumMeasured a, BeamExtremity a) 
        => Seq (PletTree a) -> (Seq (CExpr a), Seq (PletTree a))
forward = step empty . viewl where
  step acc EmptyL                           = (acc, empty)

  step acc ((S a) :< se)  | startsBeam a    = (acc, S a <| se)
                          | otherwise       = step (acc |> Atom a) (viewl se)

  step acc (p_tree :< se) | allSmall p_tree = (acc, p_tree <| se)
                          | otherwise       = step (acc |> convert1 p_tree) 
                                                   (viewl se)


backward :: (Measurement a ~ DurationMeasure, NumMeasured a, BeamExtremity a) 
         => Seq (PletTree a) -> (Seq (PletTree a), Seq (CExpr a))
backward = step empty . viewr where
  step acc EmptyR                           = (empty,acc)

  step acc (se :> (S a))  | startsBeam a    = (se |> S a, acc)
                          | otherwise       = step (Atom a <| acc) (viewr se)

  step acc (se :> p_tree) | allSmall p_tree = (se |> p_tree , acc)
                          | otherwise       = step (convert1 p_tree <| acc) 
                                                   (viewr se)




convert1 :: PletTree a -> CExpr a
convert1 (S a)           = Atom a
convert1 (Plet pm notes) = N_Plet pm (map convert1 notes)

mkBeamed :: Seq (PletTree a) -> Seq (CExpr a)
mkBeamed se = step $ viewl se where
  step EmptyL                  = empty
  step (e :< rest) | null rest = singleton $ convert1 e
  step _                       = singleton $ Beamed $ mapInto convert1 se 


startsBeam :: (Measurement a ~ DurationMeasure, NumMeasured a, BeamExtremity a) 
           => a -> Bool
startsBeam a = (nmeasure a) < quarter_note && rendersToNote a

allSmall :: (Measurement a ~ DurationMeasure, NumMeasured a)
         => PletTree a -> Bool
allSmall = pletAll (\x -> nmeasure x < quarter_note)

