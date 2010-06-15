{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleContexts           #-}
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
  
    shortenAna
  , phrase
  , phraseAna

  ) where

import Neume.Core.Duration
import Neume.Core.Metrical
import Neume.Core.ModularSyntax
import Neume.Core.Utils.Common ( modR )
import Neume.Core.Utils.HList
-- import qualified Neume.Core.Utils.Stream as S

import Data.List ( unfoldr )
import Data.Ratio


   
type Borrow = DurationMeasure


-- | A /Metrical Unit/ represents a division of a NoteList 
-- according to the sizes of a 'MeterPattern'.
--
--

type InterimBar pt = [InterimMetricalUnit pt]
type InterimMetricalUnit pt = [pt]


eighth_note :: DurationMeasure 
eighth_note = (1%8)



-- | Shorten a meter pattern to represent an anacrusis of a note 
-- or notes before the first bar. Figuratively the anacrusis 
-- /counts from the right/. 
--
-- Negative durations result in a runtime error.
--
shortenAna :: DurationMeasure -> MeterPattern -> MeterPattern
shortenAna d0 mp  | d0 == 0   = mp
                  | d0 <  0   = error "shortenAna - negative duration"
                  | otherwise = let len = sum mp in step (d0 `modR` len) len
  where
    step d len = sminus (len - d) mp 

-- If you make the stack (Stream) of meter patterns more 
-- concrete, then you can support e.g. alternating metrical
-- bars. Also supporting anacrusis becomes more direct.



--------------------------------------------------------------------------------
-- 


phrase :: (DMeasure (Division a), BeamExtremity (Division a))
       => MeterPattern -> NoteList (Division a) -> Phrase [MetricalDiv a]
phrase mp (NoteList notes) = 
    Phrase $ map (reconstituteBar . map changeInteriorMU)
           $ partitionToInterimBars mp (0,notes)


phraseAna :: (DMeasure (Division a), BeamExtremity (Division a))
          => DurationMeasure 
          -> MeterPattern 
          -> NoteList (Division a) 
          -> Phrase [MetricalDiv a]
phraseAna ana mp (NoteList notes) = 
    Phrase $ map (reconstituteBar . map changeInteriorMU)
           $ partitionWithAna ana mp notes


reconstituteBar :: InterimBar (MetricalDiv pt) -> [MetricalDiv pt]
reconstituteBar = foldr fn [] where
  fn mu acc = step mu acc 
    where
      step (x:xs) ys = x : step xs ys
      step []     ys = ys


changeInteriorMU :: (DMeasure (Division gly), BeamExtremity (Division gly))
                 => InterimMetricalUnit (Division gly) -> [MetricalDiv gly]
changeInteriorMU = 
    interior leftTest (insideAlways, insideSpeculative) mkBeam divToMDiv
  where
    leftTest e          = dmeasure e >  eighth_note || not (rendersToNote e)
    insideAlways e      = dmeasure e <= eighth_note && rendersToNote e   
    insideSpeculative e = dmeasure e <= eighth_note && not (rendersToNote e)
    
    mkBeam [a]          = a 
    mkBeam xs           = Beamed xs



divToMDiv :: Division gly -> MetricalDiv gly
divToMDiv (Unit gly)   = Atom gly
divToMDiv (Plet pm xs) = N_Plet pm (map divToMDiv xs)


--------------------------------------------------------------------------------
-- partition



partitionWithAna :: DMeasure pt 
                 => DurationMeasure -> MeterPattern -> [pt] -> [InterimBar pt]
partitionWithAna ana mp xs = bar1:bars where
  (bar1,st) = firstAna ana mp xs
  bars      = partitionToInterimBars mp st


partitionToInterimBars :: DMeasure pt 
                       => MeterPattern -> (Borrow,[pt]) -> [InterimBar pt]
partitionToInterimBars mp st0 = unfoldr phi st0 where
  phi (_,[]) = Nothing
  phi st     = Just $ nextBar mp st


firstAna :: DMeasure pt
         => DurationMeasure -> MeterPattern -> [pt] 
         -> (InterimBar pt, (Borrow,[pt]))
firstAna ana mp0 notes = nextBar (shortenAna ana mp0) (0,notes)


nextBar :: forall pt. DMeasure pt
        => MeterPattern -> (Borrow,[pt]) -> (InterimBar pt, (Borrow,[pt]))
nextBar mp (borrow,xs) = post $ foldl_st' fn (borrow,xs) id mp 
  where
    fn :: (Borrow,[pt]) -> H [pt] -> DurationMeasure -> (H [pt], (Borrow,[pt]))
    fn st accf d = let (a,st') = nextMetricalUnit d st in (accf `snocH` a,st')

    post :: (H [pt], (Borrow,[pt])) -> ([[pt]], (Borrow,[pt]))
    post (accf,st) = (accf [], st)

nextMetricalUnit :: DMeasure pt
                 => DurationMeasure
                 -> (Borrow,[pt])
                 -> ([pt], (Borrow,[pt]))
nextMetricalUnit d (borrow,xs) | borrow >= d = ([],(borrow - d,xs))
                               | otherwise   = measureSpan (d - borrow) xs

measureSpan :: DMeasure pt
            => DurationMeasure -> [pt] -> ([pt], (Borrow,[pt]))
measureSpan = step id where
  step accf d xs      | d <= 0 = (accf [], (abs d,xs))
  step accf d []               = (accf [], (abs d,[]))
  step accf d (x:xs)           = step (accf `snocH` x) (d - dmeasure x) xs

--------------------------------------------------------------------------------

-- | @interior@ models grouping notes within a metrical unit 
-- into a beam group.
--
-- First we buffer all notes longer than (1%8) etc that are on
-- the left. 
-- 
-- Once we meet an eighth note we move inside with two buffers,
-- the second buffer can be speculatively filled with rests/ spacers
-- that cannot end a beam group but can be within it. We continue
-- until we reach the end of the bar of a note longer than an
-- eighth.
--
interior :: forall a b. 
  (a -> Bool) -> (a -> Bool, a -> Bool) -> ([b] -> b) -> (a -> b) -> [a] -> [b]
interior outLeft (insideAlways, insideSpeculative) incrush fn lzt = 
    leftside id lzt
  where
    leftside :: H b -> [a] -> [b]
    leftside accf []                 = accf []  -- all left, no interior
    leftside accf (x:xs) | outLeft x = leftside (accf `snocH` fn x) xs
                         | otherwise = accf $ inside (fn x:) id xs

    -- specf is a /speculative/ buffer...
    inside :: H b -> H b -> [a] -> [b]
    inside alwaysf specf []     = (incrush $ alwaysf []) : specf []
    inside alwaysf specf (x:xs) 
      | insideSpeculative x     = inside alwaysf (specf `snocH` fn x) xs
      | insideAlways x          = inside (alwaysf . specf `snocH` fn x) id xs
      | otherwise               = (incrush $ alwaysf []) : specf (map fn (x:xs))


foldl_st' :: (st -> b -> a -> (b,st)) -> st -> b -> [a] -> (b,st) 
foldl_st' f st0 b0 lzt = step st0 b0 lzt where
    step st b []     = (b,st)
    step st b (x:xs) = let (b',st') = f st b x in b' `seq` st' `seq` step st' b' xs 