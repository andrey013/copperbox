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
import Data.Sequence hiding ( splitAt, length )
import Prelude hiding ( null )


   


-- | A Metric unit represents a division of a NoteList according
-- to the sizes of a 'MeterPattern'.
--
--
-- nop - note or plet
--

type MetricUnit   a = Seq (PletTree a) 
type SegmentState a = (DurationMeasure, NoteList a)
type BarMU        a = Seq (MetricUnit a)

mbSnoc :: Seq a -> Maybe a -> Seq a
mbSnoc se Nothing  = se
mbSnoc se (Just a) = se |> a



--------------------------------------------------------------------------------
-- 

phrase :: (Measurement a ~ DurationMeasure, NumMeasured a, BeamExtremity a) 
       => MeterPattern -> NoteList a -> StaffPhrase a
phrase mp notes = StaffPhrase $ F.toList $ fmap (StaffBar . beamBar) 
                                         $ splitToBars mp notes


phraseAna :: (Measurement a ~ DurationMeasure, NumMeasured a, BeamExtremity a) 
       => DurationMeasure -> MeterPattern -> NoteList a -> StaffPhrase a
phraseAna ana mp notes = StaffPhrase $ F.toList $ fmap (StaffBar . beamBar) 
                                                $ anaSplitToBars ana mp notes
 


--------------------------------------------------------------------------------
-- new version with snoc-able accumulator...



splitToBars :: (Measurement a ~ DurationMeasure, NumMeasured a) 
            => MeterPattern -> NoteList a -> Seq (BarMU a)
splitToBars mp notes = workerSplit mp empty (0,notes) where

anaSplitToBars :: (Measurement a ~ DurationMeasure, NumMeasured a) 
               => DurationMeasure -> MeterPattern -> NoteList a -> Seq (BarMU a)
anaSplitToBars ana mp notes = workerSplit mp (singleton bar0) state0
  where
    (bar0,state0) = firstAna ana mp notes


workerSplit :: (Measurement a ~ DurationMeasure, NumMeasured a) 
            => MeterPattern -> Seq (BarMU a) -> (SegmentState a) -> Seq (BarMU a)
workerSplit mp = step where
  step acc (_,[]) = acc
  step acc st     = let (bar,st') = nextBar mp st in step (acc |> bar) st'
  


firstAna :: (Measurement a ~ DurationMeasure, NumMeasured a) 
         => DurationMeasure -> MeterPattern -> NoteList a 
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
           -> NoteList a
           -> (MetricUnit a, SegmentState a)
nextMUnit1 = step empty where
  step acc d xs       | d <= 0 = (acc,(abs d,xs))
  step acc d []                = (acc,(abs d,[]))
  step acc d (x:xs)            = body acc d x xs

  body acc d (S a)  xs  = step (acc |> S a)    (d - nmeasure a) xs
  body acc d p_tree xs  = step (acc |> p_tree) (d - pletMeasure p_tree) xs


--------------------------------------------------------------------------------

-- This is rather horrible...

beamBar :: (Measurement a ~ DurationMeasure, NumMeasured a, BeamExtremity a) 
        => BarMU a -> [CExpr a]
beamBar = F.foldr (++) [] . fmap beamMU

beamMU :: (Measurement a ~ DurationMeasure, NumMeasured a, BeamExtremity a) 
       => MetricUnit a -> [CExpr a]
beamMU = step1 . forward where
  step1 (f,se) | null se   = f []
               | otherwise = f (step2 $ backward se)

  step2 (se,g) | null se   = g []
               | otherwise = mkBeamed se : g [] 

-- known to be not null...
mkBeamed :: Seq (PletTree a) -> CExpr a
mkBeamed se = step $ viewl se where
  step (e :< rest) | null rest = conv e
  step _                       = Beamed $ F.toList $ fmap conv se 


type H a = [a] -> [a]

atom :: H (CExpr a) -> a -> H (CExpr a)
atom f a = f . (Atom a :)

nplet :: H (CExpr a) -> PletTree a -> H (CExpr a)
nplet f a = f . (conv a :)


atomR :: a -> H (CExpr a) -> H (CExpr a)
atomR a f = (Atom a :) . f

npletR :: PletTree a -> H (CExpr a) -> H (CExpr a)
npletR a f =  (conv a :) . f


conv :: PletTree a -> CExpr a
conv (S a)           = Atom a
conv (Plet pm notes) = N_Plet pm (map conv notes)


forward :: (Measurement a ~ DurationMeasure, NumMeasured a, BeamExtremity a) 
        => Seq (PletTree a) -> (H (CExpr a), Seq (PletTree a))
forward = step id . viewl where
  step f EmptyL                           = (f, empty)

  step f ((S a) :< se)  | startsBeam a    = (f, S a <| se)
                        | otherwise       = step (f `atom` a) (viewl se)

  step f (p_tree :< se) | allSmall p_tree = (f, p_tree <| se)
                        | otherwise       = step (f `nplet` p_tree) (viewl se)


backward :: (Measurement a ~ DurationMeasure, NumMeasured a, BeamExtremity a) 
         => Seq (PletTree a) -> (Seq (PletTree a), H (CExpr a))
backward = step id . viewr where
  step f EmptyR                           = (empty,f)

  step f (se :> (S a))  | startsBeam a    = (se |> S a, f)
                        | otherwise       = step (a `atomR` f) (viewr se)

  step f (se :> p_tree) | allSmall p_tree = (se |> p_tree , f)
                        | otherwise       = step (p_tree `npletR` f) (viewr se)



startsBeam :: (Measurement a ~ DurationMeasure, NumMeasured a, BeamExtremity a) 
           => a -> Bool
startsBeam a = (nmeasure a) < quarter_note && rendersToNote a

allSmall :: (Measurement a ~ DurationMeasure, NumMeasured a)
         => PletTree a -> Bool
allSmall = pletAll (\x -> nmeasure x < quarter_note)

