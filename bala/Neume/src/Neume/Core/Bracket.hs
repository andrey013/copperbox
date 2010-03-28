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

  ) where

import Neume.Core.Duration
import Neume.Core.Metrical
import Neume.Core.NoteList
import Neume.Core.SyntaxStaff
import Neume.Core.Utils.Common

import Text.PrettyPrint.Leijen hiding ( empty )   -- package: wl-pprint

import Data.Foldable ( toList )
import Data.Ratio
import Data.Sequence hiding ( splitAt, length )
import Prelude hiding ( null )


   


-- | A Metric unit represents a division of a NoteList according
-- to the sizes of a 'MeterPattern'.
--
-- Where there is not an exact fit, the splitting algorithm will
-- borrow from the input list an extra note. This potentially 
-- means the next metrical unit might have been 'all borrowed', 
-- example a whole note in 4/4 time with meter pattern [2,2] 
-- gives:
-- 
-- > [ MUnit "wholenote", BZero ]
--
--
data MetricUnit a = MUnit (NoteList a)
                  | BZero                  -- Borrowed zero
 deriving (Eq,Show)




--------------------------------------------------------------------------------
-- 

phrase :: (Measurement a ~ DurationMeasure, NumMeasured a, BeamExtremity a) 
       => MeterPattern -> NoteList a -> StaffPhrase a
phrase mp notes = StaffPhrase $ barsFromDivisions (length mp) $ divisions 0 mp notes


barsFromDivisions :: ( Measurement a ~ DurationMeasure
                     , NumMeasured a, BeamExtremity a) 
                  => Int -> [MetricUnit a] -> [StaffBar a]
barsFromDivisions i = step . splitAt i
  where
    step (xs,[]) = [unitsToBar xs]
    step (xs,ys) = (unitsToBar xs) : step (splitAt i ys)

    fromUnit BZero         = []
    fromUnit (MUnit notes) = beam notes

    unitsToBar = StaffBar . catExprLists . map fromUnit

catExprLists :: [[CExpr a]] -> [CExpr a]
catExprLists = step . unlist1 id where
  step Nothing        = []
  step (Just ([],xs)) = step $ unlist1 id xs
  step (Just (es,xs)) = es ++ (step $ unlist1 id xs)


--------------------------------------------------------------------------------
-- Cycle the MetricalSpec / MeterPattern

-- | 'cyclePattern' : anacrusis * meter_pattern -> meter_pattern
--
-- Cycle the MeterPattern producing an infinite list.
-- 
-- The anacrusis is the length of the prefix - negative values 
-- are ignored.
--
cyclePattern :: Rational -> MeterPattern -> MeterPattern
cyclePattern ana mp | ana <= 0  = cycle mp
                    | otherwise = let len = sum mp
                                      a2  = len - (ana `modR` len)
                                  in sminus a2 (cycle mp)




--------------------------------------------------------------------------------
-- Divide the NoteList

divisions :: (Measurement a ~ DurationMeasure, NumMeasured a) 
          => Rational -> MeterPattern -> NoteList a -> [MetricUnit a]
divisions ana mp notes = step 0 (cyclePattern ana mp) notes where
  
  -- borrow greater than next mertical unit => produce a BZero
  step borrow (d:ds) xs   | borrow >= d = BZero : step (borrow-d) ds xs
 
   -- input exhausted => produce empty list
  step _      _      []   = []
 
  -- 'normal' operation => use division1 to produce a MUnit
  step borrow (d:ds) xs   = let ((one,borrow'),rest) = nextMUnit (d-borrow) xs
                            in one : step borrow' ds rest

  -- unreachable as meter patterns have been cycled
  step _       []     _    = error "Bracket.divisions - unreachable"


-- Potentially consume more than the required duration (as notes 
-- cannot be split).

nextMUnit :: (Measurement a ~ DurationMeasure, NumMeasured a)  
          => DurationMeasure
          -> NoteList a
          -> ((MetricUnit a,DurationMeasure), NoteList a)
nextMUnit dunit = post . step dunit where
  step d xs       | d <= 0 = (([],abs d),xs)
  step d []                = (([],abs d),[])
  step d (x:xs)            = body d x xs

  body d (S a)      xs = ((S a:ys,d'),rest) 
    where ((ys,d'),rest) = step (d - nmeasure a) xs
  
  body d plet_tree  xs = ((plet_tree:ys,d'),rest) 
    where ((ys,d'),rest) = step (d - pletMeasure plet_tree) xs


  post ((xs,d),rest)  = ((MUnit xs,d),rest)

--------------------------------------------------------------------------------


beam :: (Measurement a ~ DurationMeasure, NumMeasured a, BeamExtremity a) 
     => NoteList a -> [CExpr a]
beam = beamNotes

beamNotes :: (Measurement a ~ DurationMeasure, NumMeasured a, BeamExtremity a) 
     => [PletTree a] -> [CExpr a]
beamNotes = alternateUnwind out inn unbuffer
  where
    out a@(S e) 
       | pletMeasure a < quarter_note && rendersToNote e  = Nothing
    out a@(Plet _ _) 
       | pletAll ((<quarter_note) . nmeasure) a           = Nothing
    out pt                                                = Just (conv pt)
    
       
    inn a | pletAll ((<quarter_note) . nmeasure) a        = Just (conv a)
          | otherwise                                     = Nothing




conv :: PletTree a -> CExpr a
conv (S a)           = Atom a
conv (Plet pm notes) = N_Plet pm (map conv notes)



unbuffer :: BeamExtremity a => Seq (CExpr a) -> [CExpr a]
unbuffer = step [] . viewr 
  where
    step acc EmptyR     = acc
    step acc (se :> Atom a)  
        | rendersToNote a && null se = Atom a : acc
        | rendersToNote a            = (Beamed $ toList $ se |> Atom a) : acc
        | otherwise                  = step (Atom a : acc) (viewr se)  

    -- if the right edge is a n_plet always beam
    step acc (se :> bt)              = (Beamed $ toList $ se |> bt) : acc

   


alternateUnwind :: (a -> Maybe b) 
                -> (a -> Maybe interim) 
                -> (Seq interim -> [b]) 
                -> [a] -> [b]
alternateUnwind outside inside flush_buffer inp = outstep inp 
  where
    outstep []        = []
    outstep (x:xs)    = case outside x of
                         Just a  -> a : outstep xs
                         Nothing -> instep empty (x:xs)
    
    instep buf []     = flush_buffer buf
    instep buf (x:xs) = case inside x of
                          Just a  -> instep (buf |> a) xs
                          Nothing -> flush_buffer buf ++ outstep (x:xs)



--------------------------------------------------------------------------------
-- Pretty instances

instance Pretty a => Pretty (MetricUnit a) where
  pretty (MUnit notes) = angles $ pretty notes
  pretty BZero         = text "___" 


