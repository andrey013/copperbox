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

    phrase
  , phraseAna

  ) where

import Neume.Core.Duration
import Neume.Core.Metrical
import Neume.Core.SyntaxNoteList
import Neume.Core.SyntaxInterim
import Neume.Core.Utils
import Neume.Core.Utils.HList


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




--------------------------------------------------------------------------------
-- 

phrase :: (DMeasure (PletTree a), BeamExtremity (PletTree a))
       => MeterPattern -> NoteList a -> Phrase [CExpr a]
phrase mp (NoteList name notes) = 
    Phrase name $ map (reconstituteBar . map changeInteriorMU)
                $ partitionToInterimBars mp (0,notes)


phraseAna :: (DMeasure (PletTree a), BeamExtremity (PletTree a))
          => DurationMeasure -> MeterPattern -> NoteList a -> Phrase [CExpr a]
phraseAna ana mp (NoteList name notes) = undefined
    Phrase name $ map (reconstituteBar . map changeInteriorMU)
                $ partitionWithAna ana mp notes

reconstituteBar :: InterimBar (CExpr pt) -> [CExpr pt]
reconstituteBar = foldr fn [] where
  fn mu acc = step mu acc 
    where
      step (x:xs) ys = x : step xs ys
      step []     ys = ys


changeInteriorMU :: (DMeasure (PletTree gly), BeamExtremity (PletTree gly))
                 => InterimMetricalUnit (PletTree gly) -> [CExpr gly]
changeInteriorMU = 
    interior leftTest (insideAlways, insideSpeculative) mkBeam pletTreeToCExpr
  where
    leftTest e          = dmeasure e >  eighth_note || not (rendersToNote e)
    insideAlways e      = dmeasure e <= eighth_note && rendersToNote e   
    insideSpeculative e = dmeasure e <= eighth_note && not (rendersToNote e)
    
    mkBeam [a]          = a 
    mkBeam xs           = Beamed xs



pletTreeToCExpr :: PletTree gly -> CExpr gly
pletTreeToCExpr (S gly)      = Atom gly
pletTreeToCExpr (Plet pm xs) = N_Plet pm (map pletTreeToCExpr xs)


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
firstAna ana mp0 notes = nextBar (anacrusis ana mp0) (0,notes)


nextBar :: forall pt. DMeasure pt
        => MeterPattern -> (Borrow,[pt]) -> (InterimBar pt, (Borrow,[pt]))
nextBar mp (borrow,xs) = post $ foldl_st' fn (borrow,xs) id mp 
  where
    fn :: (Borrow,[pt]) -> H [pt] -> DurationMeasure -> (H [pt], (Borrow,[pt]))
    fn st accf d = let (a,st') = nextMetricalUnit d st in (accf `snoc` a,st')

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
  step accf d (x:xs)           = step (accf `snoc` x) (d - dmeasure x) xs

